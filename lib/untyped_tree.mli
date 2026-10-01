(** Nested YAML/JSON file contents, before CWL types. The only document reader.
    Does not leak [Yaml.value]. Not [Document] (CommandLineTool | Workflow) and
    not [Type.value]. *)

include module type of Data.Untyped_tree

val of_yaml_string : ?path:string -> string -> (value, Error.t) result

val load : (module FILE) -> string -> (value, Error.t) result
(** Reads the document and everything it [$import]s or [$include]s through the
    given [FILE]. A [Runtime.READ] is one. *)

val load_string : ?path:string -> string -> (value, Error.t) result
val string_field : (string * value) list -> string -> string option
val bool_field : (string * value) list -> string -> bool option
val int_field : (string * value) list -> string -> int64 option
val pp : Format.formatter -> value -> unit
