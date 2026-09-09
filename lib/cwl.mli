(** Sealed public surface. *)

module Error : module type of Error
module Doc : module type of Doc
module Schema : module type of Schema
module Type : module type of Ty
module Expr : module type of Expr
module Bind : module type of Bind

val command_line :
  tool_path:string ->
  job_path:string ->
  (string list Error.annotated, Error.t) result
