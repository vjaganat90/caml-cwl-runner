(** [inputBinding] → argv. Pure: does not open files or spawn. Expression
    evaluation is a module argument ([ENGINE]). *)

val argv :
  (module Expr.ENGINE) ->
  Command_line_tool.t ->
  Ty.object_ ->
  Expr.runtime ->
  (string list, Error.t) result
