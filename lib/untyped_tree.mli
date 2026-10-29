(** The tree a YAML or JSON text parses to, before CWL types, and the loader
    that resolves [$import], [$include] and [$base] in it (Schema Salad
    "Import", "Include" and "Explicit context"; CWL v1.2 "Document
    preprocessing"). Every document, job and [cwl.output.json] is parsed here,
    and no [Yaml.value] leaves the module. It does not interpret CWL ([Document]
    and [Ty.object_of_tree] do), fetch [http] or [https] names, split a
    [#fragment] off a name, or flatten an imported array into the array that
    holds it. Only an object holding [$import] or [$include] is replaced: every
    other key, [$base] and [$namespaces] included, stays in the tree.

    A [value] is a JSON value, except that an integer is an [Int] (an [int64]).
    An [Object] is an association list: it keeps key order and duplicate keys,
    and the field readers below use the first match.

    A [FILE] is the only effect this module has: [read_file path] is the text of
    the file at [path], or an [Error.t]. Any [Io.READ] is a [FILE]. *)

include module type of Data.Untyped_tree

val of_yaml_string : ?path:string -> string -> (value, Error.t) result
(** [of_yaml_string s] is the tree of the YAML or JSON text [s], with no
    directive resolved ([load] does that). An empty [s] is [Null], and only the
    first document of a stream is read. A plain scalar is resolved by the YAML
    1.2 core schema, as Schema Salad requires: only [true], [True], [TRUE] and
    the [false] forms are booleans, so [no] and [y] are strings; [12], [0x1F]
    and [0o17] are exact [Int]s; [2.0] and [1e3] are [Float]s; an integer
    outside the [int64] range is a [Float]. Every quoted scalar is a [String].
    Fails with [Parse] when [s] is not valid YAML, uses an alias, or has a key
    that is not a scalar; [path], when not [""], only names the file in that
    error. *)

val load : (module FILE) -> string -> (value, Error.t) result
(** [load file path] parses the file at [path] and resolves the directives in
    it, reading every file through [file]. An object holding [$import] is
    replaced by the named file, parsed and resolved in turn; one holding
    [$include], by a string with that file's text. A relative name is resolved
    against the directory of the file that holds it, or against the [$base] in
    scope: a [$base] that ends in [/] is a directory, and any other names a
    file, whose directory is used. A [file://] name is read as a path, and a
    host in it is dropped. Paths are cleaned lexically, and a [..] that has
    nothing to pop is dropped: from a relative file in the current directory,
    [../x.cwl] reads [x.cwl], and a file loaded as ["../d/a.cwl"] reads its
    relative names from [d/], not [../d/].

    An error of [read_file] is returned as it is. A file that is not valid YAML
    is a [Parse] error naming it. An [http] or [https] name is [Unsupported],
    with the feature ["remote $import"], ["remote $include"] or
    ["remote $base"], after the directive that holds it. A [Schema] error
    reports one of:

    - a [$import] or [$include] that is not a string, or both in one object; its
      path is [$import];
    - a key next to the directive other than [$base]; its path is that key;
    - a [$base] that is not a string; its path is [$base];
    - an import cycle, a file that is already being loaded ([path] included; an
      [$include] of one counts too); its path is the file. *)

val pp : Format.formatter -> value -> unit
(** [pp fmt v] prints [v] in a JSON-like form for error messages: strings are
    quoted with OCaml escapes and object keys are not quoted. The tree of
    [{"a": [1, "x"]}] prints as [{a: [1, "x"]}]. *)

val expected : string -> value -> string
(** [expected what v] is ["expected <what>, got <v>"], with [v] printed by [pp]:
    [expected "string" (Int 5L)] is ["expected string, got 5"]. The field
    readers below and [Schema.expected] use it as the message for a value of the
    wrong type. *)

val child : string -> string -> string
(** [child path key] is the json_path (see [Error]) of [key] in the object at
    [path]: [path] and [key] joined by a dot, or just [key] when [path] is [""]
    (the root). [child "inputs" "x"] is ["inputs.x"]. An array index is written
    [path[i]] by [Schema.nth], not here. *)

val string_field :
  json_path:string ->
  (string * value) list ->
  string ->
  (string option, Error.t) result
(** [string_field ~json_path kvs key] reads [key] from the fields [kvs] of the
    object at [json_path]. It is [Ok None] when [key] is missing or [null],
    [Ok (Some s)] for a string, and for any other value [v] a [Schema] error at
    [child json_path key] with the message [expected "string" v]. If [key] is
    repeated, the first one is read. *)

val bool_field :
  json_path:string ->
  (string * value) list ->
  string ->
  (bool option, Error.t) result
(** Like [string_field], for a [Bool]; the message says ["boolean"]. *)

val int_field :
  json_path:string ->
  (string * value) list ->
  string ->
  (int64 option, Error.t) result
(** Like [string_field], for an [Int]; the message says ["int"]. *)
