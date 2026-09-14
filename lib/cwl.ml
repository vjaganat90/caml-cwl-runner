module Error = Error
module Doc = Doc
module Schema = Schema
module Type = Ty
module Expr = Expr
module Bind = Bind
module Glob = Glob
module Runtime = Runtime

let ( let* ) = Error.( let* )

let command_line ~tool_path ~job_path =
  let* tool_doc = Doc.load_file tool_path in
  let* tool_ann = Schema.command_line_tool tool_doc in
  let tool = tool_ann.value in
  if tool.class_ <> "CommandLineTool" && tool.class_ <> "" then
    Error (Error.Unsupported { feature = tool.class_ })
  else
    let* job_doc = Doc.load_file job_path in
    let* job_raw = Type.object_of_doc job_doc in
    let* inputs =
      Type.apply_defaults_and_check ~inputs:(Schema.input_specs tool)
        ~job:job_raw
    in
    let cores = Option.value (Schema.cores_min tool) ~default:1. in
    let runtime = Expr.runtime_with_cores cores in
    let* argv = Bind.argv (module Expr.Param_ref) ~tool ~inputs ~runtime in
    Ok { Error.value = argv; diagnostics = tool_ann.diagnostics }

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

let resolve ~job_dir loc =
  let loc = strip_file_uri loc in
  if Filename.is_relative loc then Filename.concat job_dir loc else loc

let rec stage (module R : Runtime.RUNTIME) ~outdir ~job_dir used v =
  match v with
  | Ty.Vfile f ->
      let src =
        match f.location with
        | Some loc -> Some (resolve ~job_dir loc)
        | None -> Option.map (resolve ~job_dir) f.path
      in
      let* src =
        match src with
        | None -> rt_err "File input missing location"
        | Some s -> Ok s
      in
      let raw_base =
        match f.basename with Some b -> b | None -> Filename.basename src
      in
      let* base = safe_leaf ~what:"File basename" raw_base in
      if List.mem base used then
        rt_err (Printf.sprintf "staging collision on basename '%s'" base)
      else
        let dst = Filename.concat outdir base in
        let* () = R.copy_file ~src ~dst in
        Ok
          ( Ty.Vfile
              {
                f with
                path = Some base;
                basename = Some base;
                location = Some dst;
              },
            base :: used )
  | Ty.Vdir d ->
      let src =
        match d.location with
        | Some loc -> Some (resolve ~job_dir loc)
        | None -> Option.map (resolve ~job_dir) d.path
      in
      let* src =
        match src with
        | None -> rt_err "Directory input missing location"
        | Some s -> Ok s
      in
      (* Directory inputs keep the resolved source path; they are not copied. *)
      Ok (Ty.Vdir { location = Some src; path = Some src }, used)
  | Ty.Varray xs ->
      let rec go used acc = function
        | [] -> Ok (Ty.Varray (List.rev acc), used)
        | x :: xs ->
            let* x, used =
              stage (module R : Runtime.RUNTIME) ~outdir ~job_dir used x
            in
            go used (x :: acc) xs
      in
      go used [] xs
  | Ty.Vrecord kvs ->
      let rec go used acc = function
        | [] -> Ok (Ty.Vrecord (List.rev acc), used)
        | (k, v) :: rest ->
            let* v, used =
              stage (module R : Runtime.RUNTIME) ~outdir ~job_dir used v
            in
            go used ((k, v) :: acc) rest
      in
      go used [] kvs
  | v -> Ok (v, used)

let stage_inputs (module R : Runtime.RUNTIME) ~outdir ~job_dir inputs =
  let rec go used acc = function
    | [] -> Ok (List.rev acc)
    | (k, v) :: rest ->
        let* v, used =
          stage (module R : Runtime.RUNTIME) ~outdir ~job_dir used v
        in
        go used ((k, v) :: acc) rest
  in
  go [] [] inputs

let eval_filename ~ctx s =
  let t = String.trim s in
  let* v = Expr.Param_ref.eval ~ctx ~expr:t in
  match v with
  | Ty.Vstring name -> safe_leaf ~what:"stdout/stderr/stdin filename" name
  | other ->
      rt_err
        (Printf.sprintf "filename expression must yield string, got %s"
           (Ty.value_kind other))

let eval_glob_pattern ~ctx pat =
  let* v = Expr.Param_ref.eval ~ctx ~expr:pat in
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
    (Ty.Vfile
       {
         location = Some path;
         path = Some path;
         basename = Some (Filename.basename path);
         checksum = None;
         size = Some size;
       })

let dir_of path = Ty.Vdir { location = Some path; path = Some path }

let rec is_file_ty = function
  | Ty.File -> true
  | Ty.Union ts -> List.exists is_file_ty ts
  | _ -> false

let rec is_dir_ty = function
  | Ty.Directory -> true
  | Ty.Union ts -> List.exists is_dir_ty ts
  | _ -> false

let rec value_of_hit (module R : Runtime.RUNTIME) ~ty path =
  let dir = R.is_dir path in
  match ty with
  | Ty.Array { items; _ } ->
      value_of_hit (module R : Runtime.RUNTIME) ~ty:items path
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

let pack_hits (module R : Runtime.RUNTIME) ~id ~ty hits =
  let optional = Ty.is_optional ty in
  let inner = match ty with Ty.Array { items; _ } -> items | _ -> ty in
  let is_array = match ty with Ty.Array _ -> true | _ -> false in
  match (hits, is_array, optional) with
  | [], false, true -> Ok Ty.Vnull
  | [], false, false -> rt_err (Printf.sprintf "missing output '%s'" id)
  | [], true, _ -> Ok (Ty.Varray [])
  | [ p ], false, _ -> value_of_hit (module R : Runtime.RUNTIME) ~ty:inner p
  | ps, false, _ ->
      rt_err
        (Printf.sprintf "output '%s' matched %d paths, expected one" id
           (List.length ps))
  | ps, true, _ ->
      let* vs =
        Error.map_list (value_of_hit (module R : Runtime.RUNTIME) ~ty:inner) ps
      in
      Ok (Ty.Varray vs)

let glob_patterns ~ctx (o : Schema.output) ~stdout_name ~stderr_name =
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
              let* nested = Error.map_list (eval_glob_pattern ~ctx) glob in
              Ok (List.concat nested)))

let collect_output (module R : Runtime.RUNTIME) ~outdir ~ctx ~stdout_name
    ~stderr_name (o : Schema.output) =
  let* pats = glob_patterns ~ctx o ~stdout_name ~stderr_name in
  if pats = [] then
    if Ty.is_optional o.ty then Ok (o.id, Ty.Vnull)
    else
      match o.ty with
      | Ty.Array _ -> Ok (o.id, Ty.Varray [])
      | _ -> rt_err (Printf.sprintf "output '%s' has no glob" o.id)
  else
    let* groups =
      Error.map_list
        (fun pattern -> Glob.glob (module R) ~root:outdir ~pattern ())
        pats
    in
    let hits = List.concat groups |> List.sort_uniq String.compare in
    let* v = pack_hits (module R : Runtime.RUNTIME) ~id:o.id ~ty:o.ty hits in
    Ok (o.id, v)

let rec resolve_output_paths ~outdir v =
  let abs p = if Filename.is_relative p then Filename.concat outdir p else p in
  match v with
  | Ty.Vfile f ->
      let path =
        match f.path with
        | Some p -> Some (abs p)
        | None -> Option.map abs f.location
      in
      let location =
        match f.location with
        | Some loc -> Some (abs (strip_file_uri loc))
        | None -> path
      in
      Ty.Vfile { f with path; location }
  | Ty.Vdir d ->
      let path =
        match d.path with
        | Some p -> Some (abs p)
        | None -> Option.map abs d.location
      in
      let location =
        match d.location with
        | Some loc -> Some (abs (strip_file_uri loc))
        | None -> path
      in
      Ty.Vdir { path; location }
  | Ty.Varray xs -> Ty.Varray (List.map (resolve_output_paths ~outdir) xs)
  | Ty.Vrecord kvs ->
      Ty.Vrecord
        (List.map (fun (k, x) -> (k, resolve_output_paths ~outdir x)) kvs)
  | v -> v

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

let check_json_outputs ~outputs obj =
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

let first_unimplemented_requirement tool =
  List.find_map
    (function
      | Schema.Unimplemented { class_; in_requirements = true } -> Some class_
      | _ -> None)
    tool.Schema.requirements

let first_fatal_output_feature tool =
  List.find_map
    (fun (o : Schema.output) ->
      match o.output_binding with
      | Some { unimplemented; _ } ->
          List.find_map
            (fun d ->
              if
                d.Error.feature = "outputEval"
                || d.Error.feature = "loadContents"
              then Some d.Error.feature
              else None)
            unimplemented
      | None -> None)
    tool.Schema.outputs

let run (module R : Runtime.RUNTIME) ?outdir ~tool_path ~job_path () =
  let* tool_doc = Doc.load_file tool_path in
  let* tool_ann = Schema.command_line_tool tool_doc in
  let tool = tool_ann.value in
  if tool.class_ <> "CommandLineTool" && tool.class_ <> "" then
    Error (Error.Unsupported { feature = tool.class_ })
  else
    match first_unimplemented_requirement tool with
    | Some feature -> Error (Error.Unsupported { feature })
    | None -> (
        match first_fatal_output_feature tool with
        | Some feature -> Error (Error.Unsupported { feature })
        | None ->
            let* outdir =
              match outdir with
              | Some d ->
                  let* () = R.mkdir_p d in
                  R.abspath d
              | None -> R.mkdtemp ~prefix:"ccr-"
            in
            let* tmpdir = R.mkdtemp ~prefix:"ccr-tmp-" in
            let* tmpdir = R.abspath tmpdir in
            let job_dir = Filename.dirname job_path in
            let* job_doc = Doc.load_file job_path in
            let* job_raw = Type.object_of_doc job_doc in
            let* inputs =
              Type.apply_defaults_and_check ~inputs:(Schema.input_specs tool)
                ~job:job_raw
            in
            let* inputs =
              stage_inputs (module R : Runtime.RUNTIME) ~outdir ~job_dir inputs
            in
            let cores = Option.value (Schema.cores_min tool) ~default:1. in
            let runtime = Expr.runtime_with ~outdir ~tmpdir ~cores in
            let ctx = { Expr.inputs; self = Ty.Vnull; runtime } in
            let* argv =
              Bind.argv (module Expr.Param_ref) ~tool ~inputs ~runtime
            in
            let* stdout_name =
              match tool.stdout with
              | None ->
                  if
                    List.exists
                      (fun (o : Schema.output) -> o.stream = Schema.Stdout)
                      tool.outputs
                  then Ok (Some "cwl.stdout")
                  else Ok None
              | Some s ->
                  let* n = eval_filename ~ctx s in
                  Ok (Some n)
            in
            let* stderr_name =
              match tool.stderr with
              | None ->
                  if
                    List.exists
                      (fun (o : Schema.output) -> o.stream = Schema.Stderr)
                      tool.outputs
                  then Ok (Some "cwl.stderr")
                  else Ok None
              | Some s ->
                  let* n = eval_filename ~ctx s in
                  Ok (Some n)
            in
            let* stdin_file =
              match tool.stdin with
              | None -> Ok None
              | Some s ->
                  let* n = eval_filename ~ctx s in
                  Ok (Some (Filename.concat outdir n))
            in
            let stdout_file = Option.map (Filename.concat outdir) stdout_name in
            let stderr_file = Option.map (Filename.concat outdir) stderr_name in
            let* code =
              R.spawn ~cwd:outdir ~stdin_file ~stdout_file ~stderr_file ~argv
            in
            if not (List.mem code tool.success_codes) then
              rt_err (Printf.sprintf "command failed with exit code %d" code)
            else
              let json_path = Filename.concat outdir "cwl.output.json" in
              let* outputs =
                if R.exists json_path then
                  let* s = R.read_file json_path in
                  let* doc = Doc.load_string ~path:json_path s in
                  let* obj = Type.object_of_doc doc in
                  let obj =
                    List.map
                      (fun (k, v) -> (k, resolve_output_paths ~outdir v))
                      obj
                  in
                  check_json_outputs ~outputs:tool.outputs obj
                else
                  Error.map_list
                    (collect_output
                       (module R : Runtime.RUNTIME)
                       ~outdir ~ctx ~stdout_name ~stderr_name)
                    tool.outputs
              in
              Ok { Error.value = outputs; diagnostics = tool_ann.diagnostics })
