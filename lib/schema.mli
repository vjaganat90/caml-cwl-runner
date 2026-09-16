(** CommandLineTool and a Workflow stub as typed OCaml. Known-unimplemented
    fields become diagnostics; they are not dropped. Does not evaluate
    expressions or spawn. *)

include module type of Data.Schema

val command_line_tool :
  Untyped_tree.value -> (command_line_tool Error.annotated, Error.t) result

val document :
  Untyped_tree.value -> (Document.t Error.annotated, Error.t) result

val cores_min : command_line_tool -> float option
val input_specs : command_line_tool -> Ty.input_spec list
