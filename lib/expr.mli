(** Parameter references ([$(inputs…)], [$(self…)], [$(runtime…)]). Inline
    JavaScript is a separate [ENGINE] of the same signature. Pure: no
    filesystem. *)

include module type of Data.Expr

val default_runtime : runtime
val runtime_with_cores : float -> runtime
val runtime_with : outdir:string -> tmpdir:string -> cores:float -> runtime

module Param_ref : ENGINE
