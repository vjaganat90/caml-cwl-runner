(** [inputBinding] → argv. Pure: does not open files or spawn. Expression
    evaluation is a module argument ([ENGINE]). *)

val argv :
  (module Expr.ENGINE) ->
  Schema.command_line_tool ->
  Ty.object_ ->
  Expr.runtime ->
  (string list, Error.t) result
