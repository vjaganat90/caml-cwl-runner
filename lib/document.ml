include Data.Document
open Error.Syntax

let norm_id id =
  match String.rindex_opt id '#' with
  | None -> id
  | Some i -> String.sub id (i + 1) (String.length id - i - 1)

let entry_id path = function
  | Untyped_tree.Object kvs ->
      Untyped_tree.string_field ~json_path:path kvs "id"
  | _ -> Ok None

let with_version path version = function
  | Untyped_tree.Object kvs as obj -> (
      let+ own = Untyped_tree.string_field ~json_path:path kvs "cwlVersion" in
      match (own, version) with
      | None, Some v ->
          Untyped_tree.Object (("cwlVersion", Untyped_tree.String v) :: kvs)
      | _ -> obj)
  | other -> Ok other

let pick ~want ~path version entries =
  let want = norm_id want in
  let named i entry =
    let path = Printf.sprintf "%s[%d]" path i in
    let+ id = entry_id path entry in
    match id with
    | Some id when norm_id id = want -> [ (path, entry) ]
    | Some _ | None -> []
  in
  let* hits = Error.map_list Fun.id (List.mapi named entries) in
  match List.concat hits with
  | [ (path, one) ] -> with_version path version one
  | [] -> Error.schema "/" (Printf.sprintf "no entry %s" want)
  | _ -> Error.schema "/" (Printf.sprintf "multiple entries named %s" want)

let select ?fragment tree =
  let graph path version entries =
    let want = match fragment with Some s when s <> "" -> s | _ -> "main" in
    pick ~want ~path version entries
  in
  match tree with
  | Untyped_tree.Array entries -> graph "" None entries
  | Untyped_tree.Object kvs -> (
      match List.assoc_opt "$graph" kvs with
      | None -> Ok tree
      | Some (Untyped_tree.Array entries) ->
          let* version =
            Untyped_tree.string_field ~json_path:"" kvs "cwlVersion"
          in
          graph "$graph" version entries
      | Some _ -> Error.schema "/" "$graph must be an array")
  | other -> Ok other

let of_tree ?fragment tree =
  let* tree = select ?fragment tree in
  match tree with
  | Untyped_tree.Array _ ->
      Error.schema "/" "$graph must be an array of processes"
  | Untyped_tree.Object kvs -> (
      let* class_ = Untyped_tree.string_field ~json_path:"" kvs "class" in
      match Option.value class_ ~default:"" with
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
      | class_ -> Error.unsupported class_)
  | other -> Schema.expected ~json_path:"/" ~what:"a CWL document object" other
