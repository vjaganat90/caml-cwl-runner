module Error = Error
module Doc = Doc
module Schema = Schema
module Type = Ty
module Expr = Expr
module Bind = Bind

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
