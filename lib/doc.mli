(** YAML/JSON documents as our tree. Does not leak [Yaml.value]. *)

include module type of Data.Doc

val of_yaml_string : ?path:string -> string -> (value, Error.t) result
val load : (module FILE) -> path:string -> (value, Error.t) result
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
