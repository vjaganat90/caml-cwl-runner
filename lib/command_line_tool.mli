(** A CWL CommandLineTool record and its parser. Known-unimplemented fields
    become diagnostics; they are not dropped. Does not evaluate expressions or
    spawn. *)

include module type of Data.Command_line_tool

val of_tree : Untyped_tree.value -> (t Error.annotated, Error.t) result
val cores_min : t -> float option
val input_specs : t -> Ty.input_spec list
