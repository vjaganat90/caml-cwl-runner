(** The runner's error type [t], the [diagnostic] record, and helpers to build,
    sequence and print them. No I/O.

    A [t] is carried by the [Error] case of a [result]: the operation failed and
    has no value. The CLI exits 33 for [Unsupported] and 1 for every other [t].
    A [diagnostic] means the document uses a CWL construct that was read but is
    not implemented. The operation carries on and returns its diagnostics beside
    the value, as an ['a annotated] (a record of [value] and [diagnostics]); a
    diagnostic never fails the call that returns it. Whether one stops a run is
    decided in [Cwl]: [Cwl.run] and [Cwl.command_line] fail with [Unsupported]
    for a [Workflow]; [Cwl.run] alone fails for an unimplemented class under
    [requirements] (not [hints]) and, when it collects an output by [glob], for
    an [outputEval] or [loadContents] in that output's binding.

    The cases of [t]:

    - [Parse { path; message }]: the text is not valid YAML or JSON. [path] is
      the file it came from, when known.
    - [Schema { path; message }]: valid YAML or JSON that is not a valid CWL
      document or job. [path] says where (see json_path below).
    - [Unsupported { feature }]: the document needs a CWL feature that is not
      implemented. [feature] names it: a process class (["Workflow"],
      ["ExpressionTool"]), a requirement class (["ShellCommandRequirement"]), an
      output-binding field (["outputEval"]), ["InlineJavascriptRequirement"]
      (also for an expression that needs JavaScript), or a remote [$import],
      [$include] or [$base] (["remote $import"]).
    - [Type { param; expected; got }]: the value for [param] (an input or output
      id, or a file path) does not have its declared type. [expected] names the
      type; [got] is what was found, as a kind (["string"]) or the printed
      value.
    - [Expr { message }]: a parameter reference could not be evaluated, for
      example because it is unclosed or looks up a field or index that does not
      exist; also a [position] expression that is not an int.
    - [Missing { param }]: the required input [param] is absent or [null] in the
      job and has no default.
    - [Runtime { message }]: a failure while running the tool: file access,
      staging, globbing, collecting outputs, starting the process, or a
      non-success exit code.

    A [diagnostic] names the construct ([feature], such as
    ["ShellCommandRequirement"] or ["unknown field x"]), where it was found
    ([json_path]), the text to print ([message]), and whether it was found under
    [requirements] ([in_requirements]; [false] under [hints], elsewhere, and for
    an unknown field of a [DockerRequirement]). A field the runner does not know
    is reported the same way as one it does not implement.

    A json_path is the location of a node in a document, written as keys joined
    by dots with array indexes in brackets: [inputs.x.inputBinding.prefix],
    [inputs[0].inputBinding.separate], [$graph[0].id]. A key of the root object
    has no prefix ([class]); an error about the root itself is at ["/"]. An
    error found while resolving imports has no position: its path is the key at
    fault, such as [$import], or the file for an import cycle. *)

include module type of Data.Error

val unimplemented : ?in_requirements:bool -> string -> string -> diagnostic
(** [unimplemented ?in_requirements feature json_path] is the diagnostic that
    [feature], found at [json_path], is not implemented. Its message is
    ["<feature> is not implemented"], plus [" (requirement)"] when
    [in_requirements] (default [false]) is [true], which says the construct was
    found under [requirements]. The flag stops nothing by itself: [Cwl.run]
    checks the parsed requirement classes. *)

(** Binding operators over [(_, t) result]. [open Error.Syntax] in every module
    that sequences results. *)
module Syntax : sig
  val ( let* ) : ('a, t) result -> ('a -> ('b, t) result) -> ('b, t) result
  (** [let* x = r in body] is [Result.bind r (fun x -> body)]: [body] runs on
      the [Ok] value of [r], and an [Error] from [r] is the result unchanged. *)

  val ( let+ ) : ('a, t) result -> ('a -> 'b) -> ('b, t) result
  (** [let+ x = r in e] is [Result.map (fun x -> e) r]. Use it for the last
      step, where [e] is a plain value that cannot fail. *)
end

(** Binding operators over a result that carries diagnostics: the readers of a
    document return one, so a reader built from others collects their
    diagnostics without passing them by hand. [open Error.Annotated] where a
    whole module reads a document. *)
module Annotated : sig
  type nonrec 'a t = ('a annotated, t) result
  (** A value with the diagnostics found while reading it, or the first error.
  *)

  val return : 'a -> 'a t
  (** [return x] is [x] with no diagnostic. *)

  val warn : diagnostic list -> 'a -> 'a t
  (** [warn ds x] is [x] with the diagnostics [ds]. *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [let* x = r in body] runs [body] on the value of [r], for a read that
      depends on an earlier one. The diagnostics of [r] come before those of
      [body]; an [Error] of [r] is the result, and [body] does not run. *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** [let+ x = r in e] is [r] with its value [x] replaced by [e], and the same
      diagnostics. *)

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** [let+ x = r and+ y = s in e] reads two independent things: the diagnostics
      of [r] come before those of [s], and the error of [r] wins over that of
      [s], so a chain of [and+] reports in the order it is written. *)

  val all : 'a t list -> 'a list t
  (** [all rs] is the list of the values of [rs] with all their diagnostics, in
      order, or the first error among [rs]. *)
end

val runtime : string -> ('a, t) result
(** [runtime message] is [Error (Runtime { message })]: a failure while running
    the tool (see the cases of [t] above). *)

val schema : string -> string -> ('a, t) result
(** [schema json_path message] is
    [Error (Schema { path = json_path; message })]: the document or job is not
    valid CWL at [json_path] (see json_path above). *)

val expr : string -> ('a, t) result
(** [expr message] is [Error (Expr { message })]: a parameter reference could
    not be evaluated. *)

val unsupported : string -> ('a, t) result
(** [unsupported feature] is [Error (Unsupported { feature })]: the document
    needs a CWL feature that is not implemented. *)

val map_list : ('a -> ('b, t) result) -> 'a list -> ('b list, t) result
(** [map_list f xs] is [Ok ys], where [ys] are the results of [f] on the
    elements of [xs], in order. The first [Error] is returned and [f] is not
    applied to the elements after it. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt e] prints [e] as a message that begins with its kind: [parse error]
    ([in <path>] follows when the file is known), [schema error at <path>],
    [unsupported feature], [type error for <param>], [expression error],
    [missing required input parameter] or [runtime error]. For example
    [Schema { path = "inputs.x"; message = "expected string, got 5" }] prints
    ["schema error at inputs.x: expected string, got 5"]. *)

val to_string : t -> string
(** [to_string e] is the text [pp] prints for [e]. [ccr] writes it to stderr
    after ["ccr: "]. *)

val pp_diagnostic : Format.formatter -> diagnostic -> unit
(** [pp_diagnostic fmt d] prints ["<feature> at <json_path>: <message>"], with
    [" (requirement)"] after the path when [d.in_requirements]. So a requirement
    made by [unimplemented] prints as
    ["X at requirements[0] (requirement): X is not implemented (requirement)"],
    and a hint as ["X at hints[0]: X is not implemented"]. *)

val diagnostic_to_string : diagnostic -> string
(** [diagnostic_to_string d] is the text [pp_diagnostic] prints for [d]. [ccr]
    writes each diagnostic to stderr after ["ccr: "], unless [--quiet]. *)
