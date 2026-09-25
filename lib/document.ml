(** A CWL process file: CommandLineTool or Workflow. [of_tree] reads [class] and
    builds one or the other. Not a job input object. *)

include Data.Document

let ( let* ) = Error.( let* )
let schema_err message = Error (Error.Schema { path = "/"; message })

let norm_id id =
  match String.rindex_opt id '#' with
  | None -> id
  | Some i -> String.sub id (i + 1) (String.length id - i - 1)

let entry_id = function
  | Untyped_tree.Object kvs -> Untyped_tree.string_field kvs "id"
  | _ -> None

let with_version version = function
  | Untyped_tree.Object kvs as obj -> (
      match (Untyped_tree.string_field kvs "cwlVersion", version) with
      | None, Some v ->
          Untyped_tree.Object (("cwlVersion", Untyped_tree.String v) :: kvs)
      | _ -> obj)
  | other -> other

let pick ~want version entries =
  let want = norm_id want in
  let hits =
    List.filter
      (fun entry ->
        match entry_id entry with Some id -> norm_id id = want | None -> false)
      entries
  in
  match hits with
  | [ one ] -> Ok (with_version version one)
  | [] -> schema_err (Printf.sprintf "no entry %s" want)
  | _ -> schema_err (Printf.sprintf "multiple entries named %s" want)

let select ?fragment tree =
  let graph version entries =
    let want = match fragment with Some s when s <> "" -> s | _ -> "main" in
    pick ~want version entries
  in
  match tree with
  | Untyped_tree.Array entries -> graph None entries
  | Untyped_tree.Object kvs -> (
      match List.assoc_opt "$graph" kvs with
      | None -> Ok tree
      | Some (Untyped_tree.Array entries) ->
          graph (Untyped_tree.string_field kvs "cwlVersion") entries
      | Some _ -> schema_err "$graph must be an array")
  | other -> Ok other

let of_tree ?fragment tree =
  let* tree = select ?fragment tree in
  match tree with
  | Untyped_tree.Array _ -> schema_err "$graph must be an array of processes"
  | Untyped_tree.Object kvs -> (
      match
        Option.value (Untyped_tree.string_field kvs "class") ~default:""
      with
      | "Workflow" ->
          let* wf = Workflow.of_tree tree in
          Ok
            {
              Error.value = Workflow wf.Error.value;
              diagnostics = wf.diagnostics;
            }
      | "CommandLineTool" | "" ->
          let* clt = Command_line_tool.of_tree tree in
          Ok
            {
              Error.value = Command_line_tool clt.Error.value;
              diagnostics = clt.diagnostics;
            }
      | class_ -> Error (Error.Unsupported { feature = class_ }))
  | other ->
      schema_err
        (Format.asprintf "expected a CWL document object, got %a"
           Untyped_tree.pp other)
