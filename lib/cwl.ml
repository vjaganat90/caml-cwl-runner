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
module Location = Location
module Workdir = Workdir
module Io = Io
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

(* The string a [stdin], [stdout] or [stderr] expression gives, if [check]
   accepts it. *)
let eval_filename check ctx s =
  let t = String.trim s in
  let* v = Expr.Param_ref.eval ctx t in
  match v with
  | Untyped_tree.String name -> check name
  | other ->
      Error.runtime
        (Printf.sprintf "filename expression must yield string, got %s"
           (Ty.value_kind other))

(* CommandLineTool.yml, [stdin]: "A path to a file whose contents must be
   piped into the command's standard input stream." Any path, then: a
   relative one is taken from the designated output directory, as the tool
   would, and one under a container's maps to the host. *)
let stdin_path ctx ~container outdir s =
  let+ p = eval_filename Result.ok ctx s in
  let p = Workdir.relocate ~container ~host:outdir p in
  if Filename.is_relative p then Filename.concat outdir p else p

let eval_glob_pattern ctx pat =
  let* v = Expr.Param_ref.eval ctx pat in
  match v with
  | Untyped_tree.String s -> Ok [ s ]
  | Untyped_tree.Null -> Ok []
  | Untyped_tree.Array xs ->
      Error.map_list
        (function
          | Untyped_tree.String s -> Ok s
          | other ->
              Error.runtime
                (Printf.sprintf "glob array item must be string, got %s"
                   (Ty.value_kind other)))
        xs
  | other ->
      Error.runtime
        (Printf.sprintf "glob expression must yield string, got %s"
           (Ty.value_kind other))

(* The File or Directory found at [path], with the fields the spec computes. *)
let found cls path =
  Ty.fill_file_paths
    (Untyped_tree.Object
       [
         ("class", Untyped_tree.String cls);
         ("location", Untyped_tree.String path);
         ("path", Untyped_tree.String path);
       ])

let file_of (module R : Io.READ) path =
  let+ size = R.file_size path in
  Ty.set "size" (Untyped_tree.Int size) (found "File" path)

let dir_of path = found "Directory" path

let rec contains target = function
  | Ty.Union ts -> List.exists (contains target) ts
  | t -> t = target

let value_of_hit (module R : Io.READ) ty path =
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

(* The value of an output of type [ty] that nothing gives: null when it is
   optional, an empty array for an array, else the error [missing]. *)
let absent ty missing =
  if Ty.is_optional ty then Ok Untyped_tree.Null
  else
    match ty with
    | Ty.Array _ -> Ok (Untyped_tree.Array [])
    | _ -> Error.runtime missing

let pack_hits (module R : Io.READ) id ty hits =
  let inner = match ty with Ty.Array { items; _ } -> items | _ -> ty in
  match (hits, ty) with
  | [], _ -> absent ty (Printf.sprintf "missing output '%s'" id)
  | ps, Ty.Array _ ->
      let+ vs = Error.map_list (value_of_hit (module R) inner) ps in
      Untyped_tree.Array vs
  | [ p ], _ -> value_of_hit (module R) inner p
  | ps, _ ->
      Error.runtime
        (Printf.sprintf "output '%s' matched %d paths, expected one" id
           (List.length ps))

(* Process.yml, Directory [listing]: "List of files or subdirectories
   contained in this directory." The spec does not say how deep an output's
   listing goes; as cwltool does, a found Directory is listed to the bottom,
   sorted by name, with dot names. Each entry is confined to [roots] like a
   glob hit, and a walk deeper than [Glob.max_glob_depth] (a symlink loop)
   is [Error.Runtime]. *)
let rec listed (module R : Io.READ) roots depth v =
  match (Ty.class_of v, Ty.path v) with
  | Some "Directory", Some _ when depth > Glob.max_glob_depth ->
      Error.runtime "Directory listing exceeded the depth limit"
  | Some "Directory", Some dir ->
      let* names = R.read_dir dir in
      let entry name =
        let p = Filename.concat dir name in
        let* () = R.confined roots p in
        listed
          (module R : Io.READ)
          roots (depth + 1)
          (if R.is_dir p then dir_of p else found "File" p)
      in
      let+ entries = Error.map_list entry (List.sort String.compare names) in
      Ty.set "listing" (Untyped_tree.Array entries) v
  | _ -> Ok v

let glob_patterns ctx stdout_name stderr_name (o : Schema.output) =
  match o.stream with
  | Schema.Stdout -> (
      match stdout_name with Some n -> Ok [ n ] | None -> Ok [ "cwl.stdout" ])
  | Schema.Stderr -> (
      match stderr_name with Some n -> Ok [ n ] | None -> Ok [ "cwl.stderr" ])
  | Schema.No_stream -> (
      match o.output_binding with
      | None | Some { glob = []; _ } -> Ok []
      | Some { output_eval = Some _; _ } -> Error.unsupported "outputEval"
      | Some { glob; unimplemented; _ } -> (
          match
            List.find_opt
              (fun d -> d.Error.feature = "loadContents")
              unimplemented
          with
          | Some d -> Error.unsupported d.Error.feature
          | None ->
              let+ nested = Error.map_list (eval_glob_pattern ctx) glob in
              List.concat nested))

let typed_json_output (o : Schema.output) v =
  if Ty.matches o.ty v then Ok (o.id, v)
  else
    Error
      (Error.Type
         { param = o.id; expected = Ty.type_name o.ty; got = Ty.value_kind v })

(* CommandLineTool.yml, [outputEval], says what [self] is beside a [glob]
   only; with none, as cwltool has it, [self] is the empty array. A File or
   Directory it gives is confined like a glob hit. *)
let evaluated (module R : Io.RUNTIME) ~container ~stage_root outdir roots ctx
    (o : Schema.output) expr =
  let* v =
    Expr.Param_ref.eval { ctx with Expr.self = Untyped_tree.Array [] } expr
  in
  let* v =
    Workdir.confine (module R) roots (Workdir.resolve_paths ~container outdir v)
  in
  let* v = Workdir.export (module R) stage_root outdir v in
  let* v = Ty.map_result (listed (module R) roots 0) v in
  typed_json_output o v

let collect_output (module R : Io.RUNTIME) ~container ~stage_root outdir roots
    ctx stdout_name stderr_name (o : Schema.output) =
  match o.output_binding with
  | Some { glob = []; output_eval = Some expr; _ } when o.stream = No_stream ->
      evaluated (module R) ~container ~stage_root outdir roots ctx o expr
  | _ ->
      let* pats = glob_patterns ctx stdout_name stderr_name o in
      if pats = [] then
        let+ v = absent o.ty (Printf.sprintf "output '%s' has no glob" o.id) in
        (o.id, v)
      else
        let pats =
          List.map
            (fun pat ->
              Workdir.relocate ~container ~host:outdir (String.trim pat))
            pats
        in
        let* groups =
          Error.map_list (Glob.glob (module R) ~roots outdir) pats
        in
        let hits = List.concat groups |> List.sort_uniq String.compare in
        let* _ = Error.map_list (R.confined roots) hits in
        let* v = pack_hits (module R) o.id o.ty hits in
        let+ v = Ty.map_result (listed (module R) roots 0) v in
        (o.id, v)

let check_json_outputs outputs obj =
  let declared_ids = List.map (fun (o : Schema.output) -> o.id) outputs in
  let* checked =
    Error.map_list
      (fun (o : Schema.output) ->
        match List.assoc_opt o.id obj with
        | None ->
            let+ v =
              absent o.ty (Printf.sprintf "cwl.output.json missing '%s'" o.id)
            in
            (o.id, v)
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

(* Process.yml, File: a relative [location] "must be resolved relative to
   the IRI of the document it appears in". So a job value is located against
   the job file's directory and a [default] against the tool's, each made
   absolute, since the tool runs in another directory. With no job file, the
   job is empty. *)
let job_inputs (module R : Io.READ) tool tool_path job =
  let* tool_dir =
    R.realpath (Filename.dirname (fst (split_fragment tool_path)))
  in
  let* job_dir, job_raw =
    match job with
    | None -> Ok (tool_dir, [])
    | Some job_path ->
        let* job_tree = Untyped_tree.load (module R) job_path in
        let* job_raw = Type.object_of_tree job_tree in
        let+ job_dir = R.realpath (Filename.dirname job_path) in
        (job_dir, job_raw)
  in
  let locate_default (spec : Type.input_spec) =
    let locate d = Result.map Option.some (Workdir.locate tool_dir d) in
    let+ default = Option.fold ~none:(Ok None) ~some:locate spec.default in
    { spec with default }
  in
  let* specs =
    Error.map_list locate_default (Command_line_tool.input_specs tool)
  in
  (* A key no input declares is dropped by the check, so it is not read. *)
  let declared k = List.exists (fun (s : Type.input_spec) -> s.id = k) specs in
  let* job_raw =
    Error.map_list
      (fun (k, v) ->
        if declared k then
          let+ v = Workdir.locate job_dir v in
          (k, v)
        else Ok (k, v))
      job_raw
  in
  Type.apply_defaults_and_check specs job_raw

(* The runtime of a tool: [Local] unless its requirements select docker. *)
let runtime_for (module Local : Io.RUNTIME) docker tool =
  match docker_image tool with
  | None -> Ok (module Local : Io.RUNTIME)
  | Some image -> (
      match docker with
      | None -> Error.runtime "DockerRequirement requires a docker runtime"
      | Some mk ->
          Ok (mk ~network:(Command_line_tool.network_access tool) image))

(* Runs [f] on the tool's TMPDIR and the staging root: two fresh
   directories, by their real paths, removed when [f] returns or raises
   unless [rm_tmpdir] is [false]. *)
let with_scratch (module R : Io.RUNTIME) rm_tmpdir f =
  let fresh prefix = Result.bind (R.mkdtemp prefix) R.realpath in
  let* tmpdir = fresh "ccr-tmp-" in
  let* stage_root = fresh "ccr-stg-" in
  Fun.protect ~finally:(fun () ->
      if rm_tmpdir then
        List.iter (fun d -> ignore (R.remove_tree d)) [ tmpdir; stage_root ])
  @@ fun () -> f tmpdir stage_root

let run (module Local : Io.RUNTIME) ?docker ?outdir ?(rm_tmpdir = true)
    tool_path job =
  let* tool, diagnostics = load_command_line_tool (module Local) tool_path in
  match Schema.first_unimplemented tool.requirements with
  | Some feature -> Error.unsupported feature
  | None ->
      let* (module R : Io.RUNTIME) = runtime_for (module Local) docker tool in
      let* outdir =
        match outdir with
        | Some d ->
            let* () = R.mkdir_p d in
            R.abspath d
        | None -> R.mkdtemp "ccr-"
      in
      with_scratch (module R) rm_tmpdir @@ fun tmpdir stage_root ->
      let* inputs = job_inputs (module Local) tool tool_path job in
      let cores = Option.value (Command_line_tool.cores_min tool) ~default:1. in
      let container = designated_outdir tool outdir in
      let sources = stage_root :: Workdir.sources inputs in
      let* inputs = Workdir.stage (module R) stage_root inputs in
      let runtime = Expr.runtime_with ~outdir:container ~tmpdir ~cores in
      let ctx = { Expr.inputs; self = Untyped_tree.Null; runtime } in
      let* argv = Bind.argv (module Expr.Param_ref) tool inputs runtime in
      let stream_leaf = Workdir.safe_leaf ~what:"stdout/stderr filename" in
      let stream_filename named stream default =
        match named with
        | Some s ->
            let+ n = eval_filename stream_leaf ctx s in
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
        | Some s -> Result.map Option.some (stdin_path ctx ~container outdir s)
      in
      let stdout_file = Option.map (Filename.concat outdir) stdout_name in
      let stderr_file = Option.map (Filename.concat outdir) stderr_name in
      let* code =
        R.spawn
          { home = container; tmpdir; inputs = sources }
          outdir
          { stdin_file; stdout_file; stderr_file }
          argv
      in
      if not (List.mem code tool.success_codes) then
        Error.runtime (Printf.sprintf "command failed with exit code %d" code)
      else
        let json_path = Filename.concat outdir "cwl.output.json" in
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
                 ~container ~stage_root outdir
                 (outdir :: tmpdir :: sources)
                 ctx stdout_name stderr_name)
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
