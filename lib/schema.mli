(** What [Command_line_tool] and [Workflow] share: the process fields [inputs],
    [outputs], [requirements] and [hints] (Process in the spec) and what they
    are made of: CWL types, [CommandLineBinding], [CommandOutputBinding],
    [ResourceRequirement] and [DockerRequirement] (CommandLineTool in the spec).
    The types included below from the private [Data.Schema] are the parsed
    result: an [input] or [output] per parameter, and a [requirement] per entry
    of [requirements] or [hints]. [parse_process] reads the four fields.
    [Command_line_tool] reads the rest of a tool with the other vals, except
    [first_unimplemented], which [Cwl.run] calls. Does not parse a whole
    document, evaluate expressions (a [valueFrom] or [position] expression stays
    text) or do I/O.

    A [json_path] is the dotted location of a node in the document, such as
    [inputs[0].type] or [hints.DockerRequirement]; [""] is the document root,
    and an error about the whole document says ["/"]. An [Error.Schema] holds it
    in [path], a diagnostic in [json_path].

    A construct the runner ([ccr]) does not implement is not an error. The
    parser returns an [Error.diagnostic] beside the value it built and goes on;
    only a malformed document is an [Error.t]. An unimplemented or unknown field
    is a diagnostic too, mostly through [diagnostics_for_keys]. A type ccr
    cannot model ([record], a name it does not know) is read as [string] with a
    diagnostic. A requirement class ccr does not implement is kept as
    [Unimplemented], so that [first_unimplemented] can stop a run. *)

include module type of Data.Schema

val parse_cwl_version :
  (string * Untyped_tree.value) list -> string Error.Annotated.t
(** [parse_cwl_version kvs] is the [cwlVersion] among the fields [kvs] of a
    document object: [v1.0], [v1.1] or [v1.2], and [v1.2] if the field is
    missing, though the spec requires it at the document root. Any other
    version, or a value that is not a string ([null] too), is an [Error.Schema]
    at [cwlVersion]. Nothing branches on the version. It has no diagnostic. *)

val diag : ?in_requirements:bool -> string -> string -> Error.diagnostic
(** [diag feature json_path] is the diagnostic that [feature] is not implemented
    at [json_path]: [Error.unimplemented] under a shorter name.
    [in_requirements] (default [false]) says the construct was in
    [requirements], not in [hints]; it only marks the diagnostic, and
    [first_unimplemented] decides what stops a run. *)

val nth : string -> int -> string
(** [nth path i] is the json_path of element [i] of the array at [path]:
    [nth "inputs" 0] is [inputs[0]]. For a member of an object use
    [Untyped_tree.child], which gives [path.key]. *)

val map_i :
  (string -> 'a -> 'b Error.Annotated.t) ->
  string ->
  'a list ->
  'b list Error.Annotated.t
(** [map_i read path xs] reads every element of the array [xs] found at [path],
    the element [i] at its own path [path[i]], and returns the values with the
    diagnostics of every element, in order, or the first error. *)

val expected :
  json_path:string -> what:string -> Untyped_tree.value -> ('a, Error.t) result
(** [expected ~json_path ~what v] is always an [Error]: an [Error.Schema] at
    [json_path] whose message is [expected <what>, got <v>], with [v] printed by
    [Untyped_tree.pp]. The result type is polymorphic, so it can end any match
    arm. *)

val field :
  json_path:string ->
  (string * Untyped_tree.value) list ->
  string ->
  (string -> Untyped_tree.value -> 'a Error.Annotated.t) ->
  'a Error.Annotated.t ->
  'a Error.Annotated.t
(** [field ~json_path kvs key read absent] reads the field [key] of the object
    [kvs] found at [json_path]: [read] gets the value, [null] included, and the
    field's path [Untyped_tree.child json_path key]. A missing [key] is
    [absent], which may be an error. This is how the readers here get a field
    that [Untyped_tree.string_field] and its siblings cannot read. *)

val parse_int : string -> Untyped_tree.value -> int Error.Annotated.t
(** [parse_int json_path v] is the integer in [v]: an [Int]. Any other [v] is a
    schema error at [json_path]. A number outside the range of an OCaml [int] is
    not rejected: it wraps around. *)

val strings :
  what:string -> string -> Untyped_tree.value -> string list Error.Annotated.t
(** [strings ~what json_path v] is the list of strings that [v] gives: a string
    is a list of one, an array of strings is itself, and [null] is empty. An
    array with another element is a schema error at [json_path], not at the
    element, with the message [expected <what>, got <element>]; any other [v] is
    one with [expected <what> or array]. [what] is a noun such as ["string"]. *)

val diagnostics_for_keys :
  implemented:string list ->
  unimplemented:string list ->
  string ->
  (string * Untyped_tree.value) list ->
  Error.diagnostic list
(** [diagnostics_for_keys ~implemented ~unimplemented json_path kvs] is one
    diagnostic for each field of the object [kvs], found at [json_path], whose
    key is not in [implemented]; an [implemented] key gets none even if ccr
    drops its value (like [doc]). A key containing [':'] is reported as
    [extension <key>], one in [unimplemented] (a CWL field ccr does not
    implement) under its own name, and any other as [unknown field <key>]. The
    diagnostics are at [Untyped_tree.child json_path key], in the order of
    [kvs], with [in_requirements = false]. *)

val parse_binding : string -> Untyped_tree.value -> Ty.binding Error.Annotated.t
(** [parse_binding json_path v] reads the [CommandLineBinding] [v]
    ([inputBinding] or an object in [arguments]) into a [Ty.binding]. [position]
    is an int ([Ty.Pos]) or a string kept unevaluated ([Ty.Expr]). A missing
    field takes the spec's default, and so does a [null] [prefix], [separate],
    [itemSeparator] or [valueFrom]; a [null] [position] is a schema error. A
    [null] [v] is the default binding. A field ccr does not implement
    ([shellQuote], [loadContents], an unknown one) is a diagnostic. Anything but
    an object or [null] is a schema error at [json_path]; so is a field whose
    value has the wrong type, at that field's path. *)

val parse_process :
  (string * Untyped_tree.value) list ->
  (input list * output list * requirement list * requirement list)
  Error.Annotated.t
(** [parse_process kvs] reads [inputs], [outputs], [requirements] and [hints]
    from the document object [kvs] and returns
    [(inputs, outputs, requirements, hints)], the diagnostics in that order. A
    missing or [null] field is empty. Each field may be an array or the map form
    of the spec; in an [inputs] or [outputs] map a bare string or list is the
    type, and every [id] is cut to the part after its last [#] (or [/] if it has
    no [#]). An output of type [stdout] or [stderr] is a [File] output with
    [stream] set. Of a [ResourceRequirement] only a numeric [coresMin] is kept.

    It stops at the first error, reading the four fields in that order. The
    error is an [Error.Schema] for a malformed field, a [DockerRequirement]
    repeated in one list or a bad [dockerOutputDirectory]. For an input
    [default] that does not fit its type it is the error of [Ty.coerce], which
    names the input [id], not a json_path. A [DockerRequirement] in [hints] is
    checked the same way and kept, and also reported as a diagnostic: only
    [requirements] select docker, so the tool runs locally. *)

val first_unimplemented : requirement list -> string option
(** [first_unimplemented rs] is the class of the first [Unimplemented] entry of
    [rs] that came from [requirements], or [None]. [Cwl.run] fails with
    [Unsupported] (exit 33) on [Some], because the spec makes it a fatal error
    to run with a requirement that the implementation cannot satisfy or does not
    recognize. An entry from [hints] ([in_requirements = false]) never counts. A
    [ResourceRequirement] is never [Unimplemented]: a field of it that ccr
    ignores gives at most a diagnostic. *)
