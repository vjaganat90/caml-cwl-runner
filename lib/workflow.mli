(** A CWL Workflow record. Parsing only: graph execution is not here yet.
    [of_tree] accepts [class: Workflow] and reports it unimplemented. *)

include module type of Data.Workflow

val of_tree : Untyped_tree.value -> (t Error.annotated, Error.t) result
