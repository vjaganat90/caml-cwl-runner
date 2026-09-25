(** CommandLineTool facade: load, stage, argv, spawn, capture outputs.
    [command_line] stops at argv; [run] executes. Does not implement Workflow or
    a JavaScript engine. *)

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
module Runtime = Runtime

let ( let* ) = Error.( let* )

let load_command_line_tool tool_path =
  let* tree = Untyped_tree.load_file tool_path in
  let* doc = Document.of_tree tree in
  match doc.value with
  | Document.Command_line_tool tool -> Ok (tool, doc.diagnostics)
  | Document.Workflow _ -> Error (Error.Unsupported { feature = "Workflow" })

let command_line tool_path job_path =
  let* tool, diagnostics = load_command_line_tool tool_path in
  let* job_tree = Untyped_tree.load_file job_path in
  let* job_raw = Type.object_of_tree job_tree in
  let* inputs =
    Type.apply_defaults_and_check (Command_line_tool.input_specs tool) job_raw
  in
  let cores = Option.value (Command_line_tool.cores_min tool) ~default:1. in
  let runtime = Expr.runtime_with_cores cores in
  let* argv = Bind.argv (module Expr.Param_ref) tool inputs runtime in
  Ok { Error.value = argv; diagnostics }

let rt_err message = Error (Error.Runtime { message })

let safe_leaf ~what s =
  if s = "" || s = "." || s = ".." then
    rt_err (Printf.sprintf "%s must be a single path segment, got %S" what s)
  else if
    String.contains s '/' || String.contains s '\\' || String.contains s '\000'
  then rt_err (Printf.sprintf "%s must be a single path segment, got %S" what s)
  else Ok s

let strip_file_uri loc =
  if String.starts_with ~prefix:"file://" loc then
    let rest = String.sub loc 7 (String.length loc - 7) in
    if String.starts_with ~prefix:"//" rest then
      match String.index_from_opt rest 2 '/' with
      | Some i -> String.sub rest i (String.length rest - i)
      | None -> rest
    else rest
  else loc

let ensure_local_path s =
  match String.index_opt s ':' with
  | Some i
    when i > 0 && String.length s > i + 2 && s.[i + 1] = '/' && s.[i + 2] = '/'
    ->
      let scheme = String.lowercase_ascii (String.sub s 0 i) in
      if scheme = "file" then Ok (strip_file_uri s)
      else rt_err (Printf.sprintf "unsupported location scheme %S" scheme)
  | _ -> Ok s

let resolve ~job_dir loc =
  let loc = strip_file_uri loc in
  if Filename.is_relative loc then Filename.concat job_dir loc else loc

let stage_leaf (module R : Runtime.RUNTIME) ~container ~outdir ~job_dir used =
  function
  | Ty.Vfile f ->
      let src = Option.map (resolve ~job_dir) (Ty.file_loc f) in
      let* src =
        match src with
        | None -> rt_err "File input missing location"
        | Some s -> ensure_local_path s
      in
      let raw_base = Option.value f.basename ~default:(Filename.basename src) in
      let* base = safe_leaf ~what:"File basename" raw_base in
      if List.mem base used then
        rt_err (Printf.sprintf "staging collision on basename '%s'" base)
      else
        let dst = Filename.concat outdir base in
        let* () = R.copy_file src dst in
        let seen =
          let n = String.length container in
          let container =
            if n > 1 && container.[n - 1] = '/' then
              String.sub container 0 (n - 1)
            else container
          in
          container ^ "/" ^ base
        in
        Ok
          ( Ty.Vfile
              {
                f with
                path = Some base;
                basename = Some base;
                location = Some seen;
              },
            base :: used )
  | Ty.Vdir d ->
      let src = Option.map (resolve ~job_dir) (Ty.dir_loc d) in
      let* src =
        match src with
        | None -> rt_err "Directory input missing location"
        | Some s -> ensure_local_path s
      in
      Ok (Ty.Vdir { location = Some src; path = Some src }, used)
  | v -> Ok (v, used)

let stage (module R : Runtime.RUNTIME) ~container ~outdir ~job_dir used v =
  Ty.fold_map
    (stage_leaf (module R : Runtime.RUNTIME) ~container ~outdir ~job_dir)
    used v

let stage_inputs (module R : Runtime.RUNTIME) ~container ~outdir ~job_dir inputs
    =
  let rec go used acc = function
    | [] -> Ok (List.rev acc)
    | (k, v) :: rest ->
        let* v, used =
          stage (module R : Runtime.RUNTIME) ~container ~outdir ~job_dir used v
        in
        go used ((k, v) :: acc) rest
  in
  go [] [] inputs

let eval_filename ctx s =
  let t = String.trim s in
  let* v = Expr.Param_ref.eval ctx t in
  match v with
  | Ty.Vstring name -> safe_leaf ~what:"stdout/stderr/stdin filename" name
  | other ->
      rt_err
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
              rt_err
                (Printf.sprintf "glob array item must be string, got %s"
                   (Ty.value_kind other)))
        xs
  | other ->
      rt_err
        (Printf.sprintf "glob expression must yield string, got %s"
           (Ty.value_kind other))

let file_of (module R : Runtime.RUNTIME) path =
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

let rec is_file_ty = function
  | Ty.File -> true
  | Ty.Union ts -> List.exists is_file_ty ts
  | _ -> false

let rec is_dir_ty = function
  | Ty.Directory -> true
  | Ty.Union ts -> List.exists is_dir_ty ts
  | _ -> false

let rec value_of_hit (module R : Runtime.RUNTIME) ty path =
  let dir = R.is_dir path in
  match ty with
  | Ty.Array { items; _ } ->
      value_of_hit (module R : Runtime.RUNTIME) items path
  | _ ->
      if dir && is_dir_ty ty then Ok (dir_of path)
      else if (not dir) && is_file_ty ty then
        file_of (module R : Runtime.RUNTIME) path
      else if dir && is_file_ty ty && not (is_dir_ty ty) then
        Error
          (Error.Type { param = path; expected = "File"; got = "Directory" })
      else if (not dir) && is_dir_ty ty && not (is_file_ty ty) then
        Error
          (Error.Type { param = path; expected = "Directory"; got = "File" })
      else if dir then Ok (dir_of path)
      else file_of (module R : Runtime.RUNTIME) path

let pack_hits (module R : Runtime.RUNTIME) id ty hits =
  let optional = Ty.is_optional ty in
  let inner = match ty with Ty.Array { items; _ } -> items | _ -> ty in
  let is_array = match ty with Ty.Array _ -> true | _ -> false in
  match (hits, is_array, optional) with
  | [], false, true -> Ok Ty.Vnull
  | [], false, false -> rt_err (Printf.sprintf "missing output '%s'" id)
  | [], true, _ -> Ok (Ty.Varray [])
  | [ p ], false, _ -> value_of_hit (module R : Runtime.RUNTIME) inner p
  | ps, false, _ ->
      rt_err
        (Printf.sprintf "output '%s' matched %d paths, expected one" id
           (List.length ps))
  | ps, true, _ ->
      let* vs =
        Error.map_list (value_of_hit (module R : Runtime.RUNTIME) inner) ps
      in
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
          | Some d -> Error (Error.Unsupported { feature = d.Error.feature })
          | None ->
              let* nested = Error.map_list (eval_glob_pattern ctx) glob in
              Ok (List.concat nested)))

let trim_slash s =
  let n = String.length s in
  if n > 1 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

let normalize_abs path =
  let segs = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
  let rec go acc = function
    | [] -> Some (List.rev acc)
    | "." :: rest -> go acc rest
    | ".." :: _ -> None
    | seg :: rest -> go (seg :: acc) rest
  in
  match go [] segs with
  | None -> None
  | Some [] -> Some "/"
  | Some segs -> Some ("/" ^ String.concat "/" segs)

let relocate ~container ~host path =
  if container = host || not (String.starts_with ~prefix:"/" path) then path
  else
    match normalize_abs path with
    | None -> path
    | Some normalized ->
        let container = trim_slash container in
        let host = trim_slash host in
        if normalized = container then host
        else
          let prefix = container ^ "/" in
          if String.starts_with ~prefix normalized then
            host
            ^ String.sub normalized (String.length container)
                (String.length normalized - String.length container)
          else path

let collect_output (module R : Runtime.RUNTIME) ~container outdir roots ctx
    stdout_name stderr_name (o : Schema.output) =
  let* pats = glob_patterns ctx stdout_name stderr_name o in
  if pats = [] then
    if Ty.is_optional o.ty then Ok (o.id, Ty.Vnull)
    else
      match o.ty with
      | Ty.Array _ -> Ok (o.id, Ty.Varray [])
      | _ -> rt_err (Printf.sprintf "output '%s' has no glob" o.id)
  else
    let pats =
      List.map
        (fun pat -> relocate ~container ~host:outdir (String.trim pat))
        pats
    in
    let* groups = Error.map_list (Glob.glob (module R) ~roots outdir) pats in
    let hits = List.concat groups |> List.sort_uniq String.compare in
    let* _ = Error.map_list (R.confined roots) hits in
    let* v = pack_hits (module R : Runtime.RUNTIME) o.id o.ty hits in
    Ok (o.id, v)

let resolve_output_paths ~container outdir v =
  let abs p =
    let p = strip_file_uri p in
    let p = relocate ~container ~host:outdir p in
    if Filename.is_relative p then Filename.concat outdir p else p
  in
  Ty.map
    (function
      | Ty.Vfile f ->
          let path = Option.map abs (Ty.file_path f) in
          let location = Option.map abs f.location in
          let location =
            match location with Some _ as l -> l | None -> path
          in
          Ty.Vfile { f with path; location }
      | Ty.Vdir d ->
          let path = Option.map abs (Ty.dir_path d) in
          let location = Option.map abs d.location in
          let location =
            match location with Some _ as l -> l | None -> path
          in
          Ty.Vdir { path; location }
      | v -> v)
    v

let node_matches path expected got =
  match (expected, got) with
  | `File, `File -> Ok ()
  | `Directory, `Directory -> Ok ()
  | `File, `Directory ->
      Error (Error.Type { param = path; expected = "File"; got = "Directory" })
  | `Directory, `File ->
      Error (Error.Type { param = path; expected = "Directory"; got = "File" })
  | `File, _ ->
      Error (Error.Type { param = path; expected = "File"; got = "not a file" })
  | `Directory, _ ->
      Error
        (Error.Type
           { param = path; expected = "Directory"; got = "not a directory" })

let confine_value (module R : Runtime.RUNTIME) roots v =
  let confine_path raw =
    if raw = "" then rt_err "File or Directory output missing path"
    else
      let* () = R.confined roots raw in
      match R.realpath raw with Ok p -> Ok p | Error _ -> Ok raw
  in
  Ty.map_result
    (function
      | Ty.Vfile f ->
          let* path =
            confine_path (Option.value (Ty.file_path f) ~default:"")
          in
          let* () = node_matches path `File (R.stat path) in
          Ok
            (Ty.fill_file_paths
               (Ty.Vfile { f with path = Some path; location = Some path }))
      | Ty.Vdir d ->
          let* path = confine_path (Option.value (Ty.dir_path d) ~default:"") in
          let* () = node_matches path `Directory (R.stat path) in
          Ok (Ty.Vdir { path = Some path; location = Some path })
      | v -> Ok v)
    v

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
    | _ -> rt_err (Printf.sprintf "cwl.output.json missing '%s'" o.id)

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

let first_unimplemented_requirement (tool : Command_line_tool.t) =
  List.find_map
    (function
      | Schema.Unimplemented { class_; in_requirements = true } -> Some class_
      | _ -> None)
    tool.requirements

let docker_image (tool : Command_line_tool.t) =
  List.find_map
    (function Schema.Docker docker -> Some docker | _ -> None)
    tool.requirements

let designated_outdir (tool : Command_line_tool.t) host =
  match docker_image tool with
  | Some { output_directory = Some path; _ } ->
      Schema.Container_outdir.to_string path
  | _ -> host

let run (module Local : Runtime.RUNTIME) ?docker ?outdir tool_path job_path =
  let* tool, diagnostics = load_command_line_tool tool_path in
  match first_unimplemented_requirement tool with
  | Some feature -> Error (Error.Unsupported { feature })
  | None ->
      let* (module R : Runtime.RUNTIME) =
        match docker_image tool with
        | None -> Ok (module Local : Runtime.RUNTIME)
        | Some image -> (
            match docker with
            | None -> rt_err "DockerRequirement requires a docker runtime"
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
      let* tmpdir = R.abspath tmpdir in
      let job_dir = Filename.dirname job_path in
      let* job_tree = Untyped_tree.load_file job_path in
      let* job_raw = Type.object_of_tree job_tree in
      let* inputs =
        Type.apply_defaults_and_check
          (Command_line_tool.input_specs tool)
          job_raw
      in
      let cores = Option.value (Command_line_tool.cores_min tool) ~default:1. in
      let container = designated_outdir tool outdir in
      let* inputs =
        stage_inputs
          (module R : Runtime.RUNTIME)
          ~container ~outdir ~job_dir inputs
      in
      let runtime = Expr.runtime_with ~outdir:container ~tmpdir ~cores in
      let ctx = { Expr.inputs; self = Ty.Vnull; runtime } in
      let* argv = Bind.argv (module Expr.Param_ref) tool inputs runtime in
      let stream_filename named stream default =
        match named with
        | Some s ->
            let* n = eval_filename ctx s in
            Ok (Some n)
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
            let* n = eval_filename ctx s in
            Ok (Some (Filename.concat outdir n))
      in
      let stdout_file = Option.map (Filename.concat outdir) stdout_name in
      let stderr_file = Option.map (Filename.concat outdir) stderr_name in
      let* code =
        R.spawn
          ~env:(Runtime.tool_env ~outdir ~tmpdir)
          outdir
          { stdin_file; stdout_file; stderr_file }
          argv
      in
      if not (List.mem code tool.success_codes) then
        rt_err (Printf.sprintf "command failed with exit code %d" code)
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
            let* tree = Untyped_tree.load_string ~path:json_path s in
            let* obj = Type.object_of_tree tree in
            let obj =
              List.map
                (fun (k, v) -> (k, resolve_output_paths ~container outdir v))
                obj
            in
            let* obj = check_json_outputs tool.outputs obj in
            Error.map_list
              (fun (k, v) ->
                let* v =
                  confine_value (module R : Runtime.RUNTIME) [ outdir ] v
                in
                Ok (k, v))
              obj
          else
            Error.map_list
              (collect_output
                 (module R : Runtime.RUNTIME)
                 ~container outdir glob_roots ctx stdout_name stderr_name)
              tool.outputs
        in
        Ok { Error.value = outputs; diagnostics }
