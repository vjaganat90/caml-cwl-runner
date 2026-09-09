(** CommandLineTool as typed OCaml. Known-unimplemented fields become
    diagnostics; they are not dropped. *)

include module type of Data.Schema

val command_line_tool :
  Doc.value -> (command_line_tool Error.annotated, Error.t) result

val cores_min : command_line_tool -> float option
val input_specs : command_line_tool -> Ty.input_spec list
