(** Sealed public surface: [command_line] builds argv; [run] executes a local
    CommandLineTool. Submodules are the engine. Workflow execution and a
    JavaScript [ENGINE] are not here yet. *)

module Error : module type of Error
module Untyped_tree : module type of Untyped_tree
module Schema : module type of Schema
module Command_line_tool : module type of Command_line_tool
module Workflow : module type of Workflow
module Document : module type of Document
module Type : module type of Ty
module Expr : module type of Expr
module Bind : module type of Bind
module Glob : module type of Glob
module Runtime : module type of Runtime

val command_line :
  string -> string -> (string list Error.annotated, Error.t) result

val run :
  (module Runtime.RUNTIME) ->
  ?docker:(Schema.docker -> (module Runtime.RUNTIME)) ->
  ?outdir:string ->
  ?job:string ->
  ?rm_tmpdir:bool ->
  string ->
  (Type.object_ Error.annotated, Error.t) result
(** [run runtime ?job tool]. Without [job] the input object is empty and
    defaults resolve against the tool's directory. The tool's [TMPDIR] is
    deleted when the run ends, success or failure, unless [rm_tmpdir] is
    [false]. The outdir is kept. *)
