(** Nested YAML/JSON file contents, before CWL types. The only document reader.
    Does not leak [Yaml.value]. Not [Document] (CommandLineTool | Workflow) and
    not [Type.value]. *)

include module type of Data.Untyped_tree

val of_yaml_string : ?path:string -> string -> (value, Error.t) result

val load : (module FILE) -> string -> (value, Error.t) result
(** Reads the document and everything it [$import]s or [$include]s through the
    given [FILE]. A [Runtime.READ] is one. *)

val pp : Format.formatter -> value -> unit

val child : string -> string -> string
(** [child path key] is the JSON path of [key] under [path]. [""] is the
    document root. *)

val string_field :
  json_path:string ->
  (string * value) list ->
  string ->
  (string option, Error.t) result
(** [string_field ~json_path kvs key] is [Ok None] when [key] is missing or
    [null], and [Ok (Some s)] when it is a string. Any other value is a schema
    error at [child json_path key]. *)

val bool_field :
  json_path:string ->
  (string * value) list ->
  string ->
  (bool option, Error.t) result
(** Like [string_field], for a boolean. *)

val int_field :
  json_path:string ->
  (string * value) list ->
  string ->
  (int64 option, Error.t) result
(** Like [string_field], for an int or a float that is an integer. *)
