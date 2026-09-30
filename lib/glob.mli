(** POSIX / Python glob against an abstract filesystem. CWL outputBinding rules
    ([**], reject [..], roots) live here. Runtime only implements [FS]; this
    module does not spawn or touch Eio. *)

include module type of Data.Glob

val under : string -> string -> bool

val glob :
  (module FS) ->
  ?roots:string list ->
  string ->
  string ->
  (string list, Error.t) result
