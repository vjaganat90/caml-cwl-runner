(** The command line of a CommandLineTool, for [Cwl.command_line] and [Cwl.run].
    Implements the Input binding algorithm of invocation.md: gather the bindings
    of [arguments] and of the inputs, sort them by key, turn each into command
    line words, and put [baseCommand] first. CommandLineBinding in
    CommandLineTool.yml gives the words of one binding.

    A sort key is a list of numbers and names. The key of an argument is its
    [position] (0 for a string) and its index in [arguments]. The key of an
    input is its [position] and its id; an array input with no [inputBinding]
    has no [position] in its key. The key of an array item is the array's key,
    then the item's index, [position] and name ([xs[0]]). Numbers sort before
    strings, and strings by bytes, so an argument comes before an input of the
    same [position].

    Opens no file and spawns nothing: the [ENGINE] argument evaluates [position]
    and [valueFrom]. It assumes the inputs already match their types
    ([Ty.apply_defaults_and_check]) and takes File paths as they are, staged or
    not. *)

val argv :
  (module Expr.ENGINE) ->
  Command_line_tool.t ->
  Ty.object_ ->
  Expr.runtime ->
  (string list, Error.t) result
(** [argv (module E) tool inputs runtime] is [tool.base_command] followed by the
    words of every binding, in sort order. [inputs] is the job object with its
    defaults applied: a missing input counts as null and an undeclared one gets
    no binding. [E] evaluates each [position] and [valueFrom] against [inputs]
    and [runtime]; [self] is the whole input in a [position], the bound value in
    a [valueFrom], and null in an argument.

    A binding with a [valueFrom] binds what it gives, and a value is rendered by
    its own kind. Its declared type matters only for a union (the first member
    the value matches, else [string]), for an array (the type and binding of its
    items) and for a record (its fields).
    - A string, number, File or Directory ([path], else [location]) is its
      [prefix], if any, then its text; one word if [separate] is false.
    - [true] and a record are the [prefix] alone. [false] and null add nothing,
      and [valueFrom] is not evaluated for a null input.
    - An array with [itemSeparator] is rendered like a string, its items joined
      by it; an empty one adds nothing.
    - Any other array is its [prefix] once, then each item; an empty array adds
      nothing, not even its [prefix]. An item of the input's own array is bound
      by the [inputBinding] written in the array type ([item_binding] of
      [Ty.Array]), else by [Ty.default_binding] when the input has a binding; an
      array with neither adds nothing. An item of an array a [valueFrom] gives
      is bound by [Ty.default_binding].
    - A record is the [prefix] of its binding, if any, then each field that has
      an [inputBinding], bound like an input after the record's key with the
      field's name in place of an id. A record input with no binding still binds
      its fields; a record a [valueFrom] gives does not.
    - A string in [arguments] is evaluated like a [valueFrom], so a [$(...)] in
      it is replaced; a string without one is a word as written.

    Fails with [Error.Expr] if a [position] is neither an int nor null (null is
    0), and with the error of [E.eval] for an expression it rejects. *)
