(** POSIX glob against an abstract filesystem. CWL outputBinding rules live
    here; Runtime only implements [FS]. *)

module type FS = sig
  val exists : string -> bool
  val is_dir : string -> bool
  val read_dir : string -> (string list, Error.t) result
end

val glob :
  (module FS) -> root:string -> pattern:string -> (string list, Error.t) result
