(** Sealed public surface. *)

module Error : module type of Error
module Doc : module type of Doc
module Schema : module type of Schema
module Type : module type of Ty
module Expr : module type of Expr
module Bind : module type of Bind
module Glob : module type of Glob
module Runtime : module type of Runtime

val command_line :
  tool_path:string ->
  job_path:string ->
  (string list Error.annotated, Error.t) result

val run :
  (module Runtime.RUNTIME) ->
  ?outdir:string ->
  tool_path:string ->
  job_path:string ->
  unit ->
  (Type.object_ Error.annotated, Error.t) result
