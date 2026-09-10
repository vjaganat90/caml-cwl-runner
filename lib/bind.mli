(** InputBinding → argv. Pure. Expression evaluation is a module argument. *)

val argv :
  (module Expr.ENGINE) ->
  tool:Schema.command_line_tool ->
  inputs:Ty.object_ ->
  runtime:Expr.runtime ->
  (string list, Error.t) result
