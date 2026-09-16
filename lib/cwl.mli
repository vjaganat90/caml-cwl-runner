(** Sealed public surface: [command_line] builds argv; [run] executes a local
    CommandLineTool. Submodules are the engine. Workflow and a JavaScript
    [ENGINE] are not here yet. *)

module Error : module type of Error
module Doc : module type of Doc
module Schema : module type of Schema
module Type : module type of Ty
module Expr : module type of Expr
module Bind : module type of Bind
module Glob : module type of Glob
module Runtime : module type of Runtime

val command_line :
  string -> string -> (string list Error.annotated, Error.t) result

val run :
  (module Runtime.RUNTIME) ->
  ?outdir:string ->
  string ->
  string ->
  (Type.object_ Error.annotated, Error.t) result
