(** A CWL CommandLineTool as a record ([t]) and the parser that builds it
    (CommandLineTool in the spec). [of_tree] reads [cwlVersion], [baseCommand],
    [arguments], [stdin], [stdout], [stderr] and [successCodes] itself, and
    hands [inputs], [outputs], [requirements] and [hints] to
    [Schema.parse_process]. A top-level field the runner does not implement or
    does not know is a diagnostic, not an error; [id], [label], [doc],
    [$namespaces], [$schemas] and [$base] raise none and are not kept in [t].
    Does not evaluate expressions ([stdin], [stdout] and [stderr] stay text),
    build the command line ([Bind] does), read files or spawn.

    [baseCommand] is a string or a list of strings, and empty when missing or
    [null]. In [arguments], a string is the binding whose [valueFrom] is that
    string, and any other entry is read by [Schema.parse_binding].
    [successCodes] is an int or a list of ints, and means exit code 0 alone when
    missing or [null]; an empty list accepts no exit code. *)

include module type of Data.Command_line_tool

val of_tree : Untyped_tree.value -> (t Error.annotated, Error.t) result
(** [of_tree tree] builds the tool from the document object [tree], as it is
    after [Untyped_tree.load] has resolved its [$import] and [$include] and
    [Document.of_tree] has narrowed its [$graph] to one process. The diagnostics
    come in this order: top-level fields, [class], [arguments], then those of
    [Schema.parse_process]. A missing [class] means CommandLineTool. Another
    value is only a diagnostic here, and cannot arrive through
    [Document.of_tree], which chooses the reader by [class].

    Fails with [Error.Schema] when [tree] is not an object, has a top-level
    [$import], [$include] or [$graph], has an unsupported [cwlVersion], or has a
    field of the wrong shape. It also fails with the errors of
    [Schema.parse_process]. *)

val network_access : t -> bool
(** [network_access t] is the [networkAccess] of the first [NetworkAccess] in
    [requirements], else in [hints], else [false]. Only a Docker run acts on it:
    a local run cannot restrict the network, and the spec only says a tool must
    not assume access. *)

val cores_min : t -> float option
(** [cores_min t] is the [coresMin] of the first [ResourceRequirement] in
    [t.requirements] that has a numeric one, else the same in [t.hints], else
    [None]: requirements override hints, as in the spec. The parser drops a
    [coresMin] that is not a number, an expression included, without a
    diagnostic, so it counts as absent. [Cwl] passes this to [runtime.cores]
    unrounded, with 1 (the spec's default) when it is [None]. *)

val input_specs : t -> Ty.input_spec list
(** [input_specs t] is the id, type and default of each input of [t], in order:
    what [Ty.apply_defaults_and_check] needs to check a job object. It exists
    because [Ty] sits below [Schema] and cannot name [Schema.input]. *)
