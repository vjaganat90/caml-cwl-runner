(** A CWL process file read as one process: a [Command_line_tool] or a
    [Workflow]. It works on a tree that [Untyped_tree.load] has already
    resolved. It picks the process out of a packed document by [id] (CWL v1.2
    "Packed documents"; an array at the root is an implicit [$graph], Schema
    Salad "Document graph") and reads it by [class]. It does not read files,
    resolve [$import], read a job, or run anything. A [Workflow] is read only
    for its [cwlVersion], [inputs], [outputs], [requirements] and [hints], and
    carries a [Workflow] diagnostic; [Cwl] refuses to run it. *)

include module type of Data.Document

val of_tree :
  ?fragment:string -> Untyped_tree.value -> (t Error.annotated, Error.t) result
(** [of_tree ?fragment tree] reads the one process in [tree], with the
    diagnostics of the reader for its class. If [tree] has a [$graph], or is
    itself an array, the process is the entry whose [id] is [fragment] (["main"]
    when absent or empty); entries without an [id] never match, and ids are
    compared after their last [#], so ["main"], ["#main"] and ["f.cwl#main"] are
    one id. Without a [$graph], [tree] is the process and [fragment] is ignored.
    [Cwl] passes the text after the last [#] of the process path as [fragment].
    A [$graph] entry gets the [cwlVersion] of the root object unless it has its
    own.

    The process's [class] picks [Command_line_tool] (also when [class] is
    missing) or [Workflow]; any other class is [Unsupported]. A [Schema] error
    at ["/"] reports that [tree] is not an object, that [$graph] is not an
    array, or that no entry, or more than one, has the [id]. A [class], [id] or
    [cwlVersion] that is not a string is a [Schema] error at that key. Errors of
    the class reader, such as an unsupported [cwlVersion], pass through. *)
