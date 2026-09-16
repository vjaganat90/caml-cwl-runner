(** Shared process vocabulary and the parsers both process classes use: inputs,
    outputs, requirements, types, bindings. Does not parse a whole
    CommandLineTool or Workflow and does not spawn. *)

include module type of Data.Schema

val schema_err : string -> string -> ('a, Error.t) result
val diag : ?in_requirements:bool -> string -> string -> Error.diagnostic
val nth : string -> int -> string

val map_i :
  (int -> 'a -> ('b * Error.diagnostic list, Error.t) result) ->
  'a list ->
  ('b list * Error.diagnostic list, Error.t) result

val parse_opt :
  (json_path:string ->
  Untyped_tree.value ->
  ('a list * 'b list, Error.t) result) ->
  string ->
  (string * Untyped_tree.value) list ->
  ('a list * 'b list, Error.t) result

val diagnostics_for_keys :
  implemented:string list ->
  known:string list ->
  json_path:string ->
  (string * Untyped_tree.value) list ->
  Error.diagnostic list

val parse_binding :
  json_path:string ->
  Untyped_tree.value ->
  (binding * Error.diagnostic list, Error.t) result

val parse_inputs :
  json_path:string ->
  Untyped_tree.value ->
  (input list * Error.diagnostic list, Error.t) result

val parse_outputs :
  json_path:string ->
  Untyped_tree.value ->
  (output list * Error.diagnostic list, Error.t) result

val parse_req_list :
  json_path:string ->
  in_requirements:bool ->
  Untyped_tree.value ->
  (requirement list * Error.diagnostic list, Error.t) result
