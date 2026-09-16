(** POSIX / Python glob against an abstract filesystem. CWL outputBinding rules
    ([**], reject [..], roots) live here. Runtime only implements [FS]; this
    module does not spawn or touch Eio. *)

module type FS = sig
  val exists : string -> bool
  val is_dir : string -> bool
  val read_dir : string -> (string list, Error.t) result
  val realpath : string -> (string, Error.t) result
end

val under : string -> string -> bool

val glob :
  (module FS) ->
  ?roots:string list ->
  string ->
  string ->
  (string list, Error.t) result
