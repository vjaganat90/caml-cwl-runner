(** A CWL Workflow as a record ([t]) that holds only the fields every process
    has (Workflow in the spec). Graph execution is not implemented: [of_tree]
    reads [cwlVersion], [inputs], [outputs], [requirements] and [hints], ignores
    [steps] and every other top-level field, and always reports [Workflow] as a
    diagnostic. [Cwl.run] and [Cwl.command_line] then fail with [Unsupported],
    and the CLI exits with 33 on that error. *)

include module type of Data.Workflow

val of_tree : Untyped_tree.value -> (t Error.annotated, Error.t) result
(** [of_tree tree] is the workflow in the document object [tree], with the
    diagnostic for feature ["Workflow"] (at [class]) first and then those of
    [Schema.parse_process], which reports a field only a workflow output has,
    such as [outputSource], as an unknown field. It does not look at [class]:
    [Document.of_tree] calls it for [class: Workflow]. Fails with [Error.Schema]
    when [tree] is not an object or its [cwlVersion] is bad, and with the errors
    of [Schema.parse_process]. *)
