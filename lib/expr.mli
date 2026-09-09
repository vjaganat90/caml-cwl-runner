(** Parameter references. Javascript is a later [ENGINE]. *)

include module type of Data.Expr

val default_runtime : runtime
val runtime_with_cores : float -> runtime

module Param_ref : ENGINE
