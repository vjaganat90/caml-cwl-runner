(** Avro-ish CWL types and runtime values. Nested [inputBinding] on arrays lives
    here so [Schema] can depend on [Type] without a cycle. Walkers ([map],
    [map_result], [fold], [fold_map]) recurse into arrays and records; callers
    handle leaves. Does not load documents or spawn. *)

include module type of Data.Type

val default_binding : binding
val is_optional : t -> bool
val matches : t -> value -> bool
val type_name : t -> string
val value_kind : value -> string
val file_loc : file -> string option
val dir_loc : directory -> string option
val file_path : file -> string option
val dir_path : directory -> string option
val map : (value -> value) -> value -> value

val map_result :
  (value -> (value, Error.t) result) -> value -> (value, Error.t) result

val fold : ('a -> value -> 'a) -> 'a -> value -> 'a

val fold_map :
  ('a -> value -> (value * 'a, Error.t) result) ->
  'a ->
  value ->
  (value * 'a, Error.t) result

val fill_file_paths : value -> value
val lookup : string -> object_ -> value option
val value_of_tree : string -> t -> Untyped_tree.value -> (value, Error.t) result
val object_of_tree : Untyped_tree.value -> (object_, Error.t) result

val apply_defaults_and_check :
  input_spec list -> object_ -> (object_, Error.t) result

val string_of_value : value -> string
val to_json : value -> string
val object_to_json : object_ -> string
