(** Nested YAML/JSON file contents, before CWL types. The only file reader. Does
    not leak [Yaml.value]. Default [FILE] is [In_channel]. Not [Document]
    (CommandLineTool | Workflow) and not [Type.value]. *)

include module type of Data.Untyped_tree

val of_yaml_string : ?path:string -> string -> (value, Error.t) result
val load : (module FILE) -> string -> (value, Error.t) result
val load_file : string -> (value, Error.t) result
val load_string : ?path:string -> string -> (value, Error.t) result
val assoc : string -> value -> value option
val object_fields : value -> (string * value) list option
val as_string : value -> string option
val as_bool : value -> bool option
val as_int : value -> int64 option
val as_float : value -> float option
val as_list : value -> value list option
val is_null : value -> bool
val string_field : (string * value) list -> string -> string option
val bool_field : (string * value) list -> string -> bool option
val int_field : (string * value) list -> string -> int64 option
val pp : Format.formatter -> value -> unit
