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
    cannot model ([record], [enum], [Any], a name it does not know) is read as
    [string] with a diagnostic. A requirement class ccr does not implement is
    kept as [Unimplemented], so that [first_unimplemented] can stop a run. *)

include module type of Data.Schema

val parse_cwl_version :
  (string * Untyped_tree.value) list -> (string, Error.t) result
(** [parse_cwl_version kvs] is the [cwlVersion] among the fields [kvs] of a
    document object: [v1.0], [v1.1] or [v1.2], and [v1.2] if the field is
    missing, though the spec requires it at the document root. Any other
    version, or a value that is not a string ([null] too), is an [Error.Schema]
    at [cwlVersion]. Nothing branches on the version. *)

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
  (int -> 'a -> ('b * Error.diagnostic list, Error.t) result) ->
  'a list ->
  ('b list * Error.diagnostic list, Error.t) result
(** [map_i f xs] is [List.mapi f xs] for an [f] that can fail and report
    diagnostics: [Ok (ys, diags)] holds the results and the diagnostics of every
    element, in order. The first [Error] from [f] stops it and is returned;
    earlier diagnostics are lost. The index lets [f] build the element's path
    with [nth]. *)

val expected :
  json_path:string -> what:string -> Untyped_tree.value -> ('a, Error.t) result
(** [expected ~json_path ~what v] is always an [Error]: an [Error.Schema] at
    [json_path] whose message is [expected <what>, got <v>], with [v] printed by
    [Untyped_tree.pp]. The result type is polymorphic, so it can end any match
    arm. *)

val parse_int : string -> Untyped_tree.value -> (int, Error.t) result
(** [parse_int json_path v] is the integer in [v]: an [Int]. Any other [v] is a
    schema error at [json_path]. A number outside the range of an OCaml [int] is
    not rejected: it wraps around. *)

val strings :
  json_path:string ->
  expected:string ->
  Untyped_tree.value list ->
  (string list, Error.t) result
(** [strings ~json_path ~expected xs] is [xs] as strings when every element is a
    string. The first element that is not gives a schema error at [json_path],
    not at the element, with the message [expected <expected>, got <element>];
    [expected] is a noun such as ["string"]. *)

val parse_opt :
  (json_path:string ->
  Untyped_tree.value ->
  ('a list * 'b list, Error.t) result) ->
  string ->
  (string * Untyped_tree.value) list ->
  ('a list * 'b list, Error.t) result
(** [parse_opt parse key kvs] is [parse ~json_path:key v] for the value [v] of
    field [key] in [kvs], where [parse] returns a list of items and a list of
    diagnostics; an error of [parse] is returned as is. A missing or [null]
    field is [Ok ([], [])]. [key] is also the json_path, because the fields this
    serves ([inputs], [outputs], [requirements], [hints], [arguments]) are
    top-level. *)

val diagnostics_for_keys :
  implemented:string list ->
  known:string list ->
  json_path:string ->
  (string * Untyped_tree.value) list ->
  Error.diagnostic list
(** [diagnostics_for_keys ~implemented ~known ~json_path kvs] is one diagnostic
    for each field of the object [kvs], found at [json_path], whose key is not
    in [implemented]; an [implemented] key gets none even if ccr drops its value
    (like [doc]). A key containing [':'] is reported as [extension <key>], one
    in [known] (a CWL field ccr does not implement) under its own name, and any
    other as [unknown field <key>]. The diagnostics are at
    [Untyped_tree.child json_path key], in the order of [kvs], with
    [in_requirements = false]. *)

val parse_binding :
  json_path:string ->
  Untyped_tree.value ->
  (Ty.binding * Error.diagnostic list, Error.t) result
(** [parse_binding ~json_path v] reads the [CommandLineBinding] [v]
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
  ( (input list * output list * requirement list * requirement list)
    * Error.diagnostic list,
    Error.t )
  result
(** [parse_process kvs] reads [inputs], [outputs], [requirements] and [hints]
    from the document object [kvs] and returns
    [((inputs, outputs, requirements, hints), diagnostics)], the diagnostics in
    that order. A missing or [null] field is empty. Each field may be an array
    or the map form of the spec; in an [inputs] or [outputs] map a bare string
    is the type (a bare list is not accepted), and every [id] is cut to the part
    after its last [#] (or [/] if it has no [#]). An output of type [stdout] or
    [stderr] is a [File] output with [stream] set. Of a [ResourceRequirement]
    only a numeric [coresMin] is kept.

    It stops at the first error, reading the four fields in that order. The
    error is an [Error.Schema] for a malformed field, a [DockerRequirement]
    repeated in one list or a bad [dockerOutputDirectory]. For an input
    [default] that does not fit its type it is the error of [Ty.value_of_tree],
    which names the input [id], not a json_path. A [DockerRequirement] in
    [hints] is checked the same way and kept, and also reported as a diagnostic:
    only [requirements] select docker, so the tool runs locally. *)

val first_unimplemented : requirement list -> string option
(** [first_unimplemented rs] is the class of the first [Unimplemented] entry of
    [rs] that came from [requirements], or [None]. [Cwl.run] fails with
    [Unsupported] (exit 33) on [Some], because the spec makes it a fatal error
    to run with a requirement that the implementation cannot satisfy or does not
    recognize. An entry from [hints] ([in_requirements = false]) never counts. A
    [ResourceRequirement] is never [Unimplemented]: a field of it that ccr
    ignores gives at most a diagnostic. *)
