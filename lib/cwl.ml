(** CommandLineTool facade: load, argv, spawn, capture outputs. Staging,
    relocation, confinement, and checksums are [Workdir]. [command_line] stops
    at argv; [run] executes. Does not implement Workflow or a JavaScript engine.
*)

module Error = Error
module Untyped_tree = Untyped_tree
module Schema = Schema
module Command_line_tool = Command_line_tool
module Workflow = Workflow
module Document = Document
module Type = Ty
module Expr = Expr
module Bind = Bind
module Glob = Glob
module Workdir = Workdir
module Runtime = Runtime
open Error.Syntax

let split_fragment path =
  match String.rindex_opt path '#' with
  | None -> (path, None)
  | Some i ->
      let file = String.sub path 0 i in
      let frag = String.sub path (i + 1) (String.length path - i - 1) in
      if file = "" || frag = "" then (path, None) else (file, Some frag)

let load_command_line_tool (module F : Untyped_tree.FILE) tool_path =
  let path, fragment = split_fragment tool_path in
  let* tree = Untyped_tree.load (module F) path in
  let* doc = Document.of_tree ?fragment tree in
  match doc.value with
  | Document.Command_line_tool tool -> Ok (tool, doc.diagnostics)
  | Document.Workflow _ -> Error.unsupported "Workflow"

let command_line (module F : Untyped_tree.FILE) tool_path job_path =
  let* tool, diagnostics = load_command_line_tool (module F) tool_path in
  let* job_tree = Untyped_tree.load (module F) job_path in
  let* job_raw = Type.object_of_tree job_tree in
  let* inputs =
    Type.apply_defaults_and_check (Command_line_tool.input_specs tool) job_raw
  in
  let cores = Option.value (Command_line_tool.cores_min tool) ~default:1. in
  let runtime = Expr.runtime_with_cores cores in
  let+ argv = Bind.argv (module Expr.Param_ref) tool inputs runtime in
  { Error.value = argv; diagnostics }

let eval_filename ctx s =
  let t = String.trim s in
  let* v = Expr.Param_ref.eval ctx t in
  match v with
  | Ty.Vstring name ->
      Workdir.safe_leaf ~what:"stdout/stderr/stdin filename" name
  | other ->
      Error.runtime
        (Printf.sprintf "filename expression must yield string, got %s"
           (Ty.value_kind other))

let eval_glob_pattern ctx pat =
  let* v = Expr.Param_ref.eval ctx pat in
  match v with
  | Ty.Vstring s -> Ok [ s ]
  | Ty.Vnull -> Ok []
  | Ty.Varray xs ->
      Error.map_list
        (function
          | Ty.Vstring s -> Ok s
          | other ->
              Error.runtime
                (Printf.sprintf "glob array item must be string, got %s"
                   (Ty.value_kind other)))
        xs
  | other ->
      Error.runtime
        (Printf.sprintf "glob expression must yield string, got %s"
           (Ty.value_kind other))

let file_of (module R : Runtime.READ) path =
  let* size = R.file_size path in
  Ok
    (Ty.fill_file_paths
       (Ty.Vfile
          {
            location = Some path;
            path = Some path;
            basename = Some (Filename.basename path);
            nameroot = None;
            nameext = None;
            checksum = None;
            size = Some size;
          }))

let dir_of path = Ty.Vdir { location = Some path; path = Some path }

let rec contains target = function
  | Ty.Union ts -> List.exists (contains target) ts
  | t -> t = target

let value_of_hit (module R : Runtime.READ) ty path =
  let rec go ty path =
    match ty with
    | Ty.Array { items; _ } -> go items path
    | _ -> (
        let dir = R.is_dir path in
        let file_ty = contains Ty.File ty in
        let dir_ty = contains Ty.Directory ty in
        match (dir, dir_ty, file_ty) with
        | true, true, _ -> Ok (dir_of path)
        | false, _, true -> file_of (module R) path
        | true, false, true ->
            Error
              (Error.Type { param = path; expected = "File"; got = "Directory" })
        | false, true, false ->
            Error
              (Error.Type { param = path; expected = "Directory"; got = "File" })
        | true, false, false -> Ok (dir_of path)
        | false, false, false -> file_of (module R) path)
  in
  go ty path

let pack_hits (module R : Runtime.READ) id ty hits =
  let optional = Ty.is_optional ty in
  let inner = match ty with Ty.Array { items; _ } -> items | _ -> ty in
  let is_array = match ty with Ty.Array _ -> true | _ -> false in
  match (hits, is_array, optional) with
  | [], false, true -> Ok Ty.Vnull
  | [], false, false -> Error.runtime (Printf.sprintf "missing output '%s'" id)
  | [], true, _ -> Ok (Ty.Varray [])
  | [ p ], false, _ -> value_of_hit (module R) inner p
  | ps, false, _ ->
      Error.runtime
        (Printf.sprintf "output '%s' matched %d paths, expected one" id
           (List.length ps))
  | ps, true, _ ->
      let* vs = Error.map_list (value_of_hit (module R) inner) ps in
      Ok (Ty.Varray vs)

let glob_patterns ctx stdout_name stderr_name (o : Schema.output) =
  match o.stream with
  | Schema.Stdout -> (
      match stdout_name with Some n -> Ok [ n ] | None -> Ok [ "cwl.stdout" ])
  | Schema.Stderr -> (
      match stderr_name with Some n -> Ok [ n ] | None -> Ok [ "cwl.stderr" ])
  | Schema.No_stream -> (
      match o.output_binding with
      | None | Some { glob = []; _ } -> Ok []
      | Some { glob; unimplemented } -> (
          match
            List.find_opt
              (fun d ->
                d.Error.feature = "outputEval"
                || d.Error.feature = "loadContents")
              unimplemented
          with
          | Some d -> Error.unsupported d.Error.feature
          | None ->
              let+ nested = Error.map_list (eval_glob_pattern ctx) glob in
              List.concat nested))

let collect_output (module R : Runtime.READ) ~container outdir roots ctx
    stdout_name stderr_name (o : Schema.output) =
  let* pats = glob_patterns ctx stdout_name stderr_name o in
  if pats = [] then
    if Ty.is_optional o.ty then Ok (o.id, Ty.Vnull)
    else
      match o.ty with
      | Ty.Array _ -> Ok (o.id, Ty.Varray [])
      | _ -> Error.runtime (Printf.sprintf "output '%s' has no glob" o.id)
  else
    let pats =
      List.map
        (fun pat -> Workdir.relocate ~container ~host:outdir (String.trim pat))
        pats
    in
    let* groups = Error.map_list (Glob.glob (module R) ~roots outdir) pats in
    let hits = List.concat groups |> List.sort_uniq String.compare in
    let* _ = Error.map_list (R.confined roots) hits in
    let+ v = pack_hits (module R) o.id o.ty hits in
    (o.id, v)

let collect_dir_roots acc v =
  Ty.fold
    (fun acc -> function
      | Ty.Vdir d -> (
          match Ty.dir_path d with Some p -> p :: acc | None -> acc)
      | _ -> acc)
    acc v

let missing_json_output (o : Schema.output) =
  if Ty.is_optional o.ty then Ok (o.id, Ty.Vnull)
  else
    match o.ty with
    | Ty.Array _ -> Ok (o.id, Ty.Varray [])
    | _ -> Error.runtime (Printf.sprintf "cwl.output.json missing '%s'" o.id)

let typed_json_output (o : Schema.output) v =
  if Ty.matches o.ty v then Ok (o.id, v)
  else
    Error
      (Error.Type
         { param = o.id; expected = Ty.type_name o.ty; got = Ty.value_kind v })

let check_json_outputs outputs obj =
  let declared_ids = List.map (fun (o : Schema.output) -> o.id) outputs in
  let* checked =
    Error.map_list
      (fun (o : Schema.output) ->
        match List.assoc_opt o.id obj with
        | None -> missing_json_output o
        | Some v -> typed_json_output o v)
      outputs
  in
  let extras = List.filter (fun (k, _) -> not (List.mem k declared_ids)) obj in
  Ok (checked @ extras)

let docker_image (tool : Command_line_tool.t) =
  List.find_map
    (function Schema.Docker docker -> Some docker | _ -> None)
    tool.requirements

let designated_outdir (tool : Command_line_tool.t) host =
  match docker_image tool with
  | Some { output_directory = Some path; _ } -> (path :> string)
  | _ -> host

let run (module Local : Runtime.RUNTIME) ?docker ?outdir ?(rm_tmpdir = true)
    tool_path job =
  let* tool, diagnostics = load_command_line_tool (module Local) tool_path in
  match Schema.first_unimplemented tool.requirements with
  | Some feature -> Error.unsupported feature
  | None ->
      let* (module R : Runtime.RUNTIME) =
        match docker_image tool with
        | None -> Ok (module Local : Runtime.RUNTIME)
        | Some image -> (
            match docker with
            | None ->
                Error.runtime "DockerRequirement requires a docker runtime"
            | Some mk -> Ok (mk image))
      in
      let* outdir =
        match outdir with
        | Some d ->
            let* () = R.mkdir_p d in
            R.abspath d
        | None -> R.mkdtemp "ccr-"
      in
      let* tmpdir = R.mkdtemp "ccr-tmp-" in
      let* tmpdir = R.realpath tmpdir in
      Fun.protect ~finally:(fun () ->
          if rm_tmpdir then ignore (R.remove_tree tmpdir))
      @@ fun () ->
      let* job_dir, job_raw =
        match job with
        | None -> Ok (Filename.dirname (fst (split_fragment tool_path)), [])
        | Some job_path ->
            let* job_tree = Untyped_tree.load (module Local) job_path in
            let+ job_raw = Type.object_of_tree job_tree in
            (Filename.dirname job_path, job_raw)
      in
      let* inputs =
        Type.apply_defaults_and_check
          (Command_line_tool.input_specs tool)
          job_raw
      in
      let cores = Option.value (Command_line_tool.cores_min tool) ~default:1. in
      let container = designated_outdir tool outdir in
      let* inputs =
        Workdir.stage (module R) ~container ~outdir ~job_dir inputs
      in
      let runtime = Expr.runtime_with ~outdir:container ~tmpdir ~cores in
      let ctx = { Expr.inputs; self = Ty.Vnull; runtime } in
      let* argv = Bind.argv (module Expr.Param_ref) tool inputs runtime in
      let stream_filename named stream default =
        match named with
        | Some s ->
            let+ n = eval_filename ctx s in
            Some n
        | None ->
            if
              List.exists
                (fun (o : Schema.output) -> o.stream = stream)
                tool.outputs
            then Ok (Some default)
            else Ok None
      in
      let* stdout_name =
        stream_filename tool.stdout Schema.Stdout "cwl.stdout"
      in
      let* stderr_name =
        stream_filename tool.stderr Schema.Stderr "cwl.stderr"
      in
      let* stdin_file =
        match tool.stdin with
        | None -> Ok None
        | Some s ->
            let+ n = eval_filename ctx s in
            Some (Filename.concat outdir n)
      in
      let stdout_file = Option.map (Filename.concat outdir) stdout_name in
      let stderr_file = Option.map (Filename.concat outdir) stderr_name in
      let* code =
        R.spawn
          { home = container; tmpdir }
          outdir
          { stdin_file; stdout_file; stderr_file }
          argv
      in
      if not (List.mem code tool.success_codes) then
        Error.runtime (Printf.sprintf "command failed with exit code %d" code)
      else
        let json_path = Filename.concat outdir "cwl.output.json" in
        let glob_roots =
          outdir :: tmpdir
          :: List.fold_left
               (fun acc (_, v) -> collect_dir_roots acc v)
               [] inputs
        in
        let* outputs =
          if R.exists json_path then
            let* s = R.read_file json_path in
            let* tree = Untyped_tree.of_yaml_string ~path:json_path s in
            let* obj = Type.object_of_tree tree in
            let obj =
              List.map
                (fun (k, v) -> (k, Workdir.resolve_paths ~container outdir v))
                obj
            in
            let* obj = check_json_outputs tool.outputs obj in
            Error.map_list
              (fun (k, v) ->
                let* v = Workdir.confine (module R) [ outdir ] v in
                Ok (k, v))
              obj
          else
            Error.map_list
              (collect_output
                 (module R)
                 ~container outdir glob_roots ctx stdout_name stderr_name)
              tool.outputs
        in
        let* outputs =
          Error.map_list
            (fun (k, v) ->
              let+ v = Workdir.checksum (module R) v in
              (k, v))
            outputs
        in
        Ok { Error.value = outputs; diagnostics }
