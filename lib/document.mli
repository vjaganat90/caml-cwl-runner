(** A CWL process file: CommandLineTool or Workflow. [of_tree] reads [class] and
    builds one or the other. Not a job input object. *)

include module type of Data.Document

val of_tree : Untyped_tree.value -> (t Error.annotated, Error.t) result
