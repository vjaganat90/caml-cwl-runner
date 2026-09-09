(** Avro-ish CWL types and runtime values. Nested [inputBinding] on arrays lives
    here so [Schema] can depend on [Type] without a cycle. *)

include module type of Data.Type

val default_binding : binding
val is_optional : t -> bool
val type_name : t -> string
val value_kind : value -> string
val fill_file_paths : value -> value
val lookup : string -> object_ -> value option
val value_of_doc : param:string -> ty:t -> Doc.value -> (value, Error.t) result
val object_of_doc : Doc.value -> (object_, Error.t) result

val apply_defaults_and_check :
  inputs:input_spec list -> job:object_ -> (object_, Error.t) result

val string_of_value : value -> string
