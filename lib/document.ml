(** A CWL process file: CommandLineTool or Workflow. [of_tree] reads [class] and
    builds one or the other. Not a job input object. *)

include Data.Document

let ( let* ) = Error.( let* )
let schema_err message = Error (Error.Schema { path = "/"; message })

let of_tree tree =
  match tree with
  | Untyped_tree.Array _ ->
      schema_err "$graph / packed documents are not implemented"
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
