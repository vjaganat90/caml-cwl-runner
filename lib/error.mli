(** Failure of the current operation ([t]) vs a parsed construct that is not
    implemented ([diagnostic]). [in_requirements] is fatal at execute, not at
    argv. No I/O. *)

include module type of Data.Error

val unimplemented : ?in_requirements:bool -> string -> string -> diagnostic

(** [open Error.Syntax] in every module that sequences results. *)
module Syntax : sig
  val ( let* ) : ('a, t) result -> ('a -> ('b, t) result) -> ('b, t) result
  (** Sequence: the rest needs this value. The first [Error] stops it. *)

  val ( let+ ) : ('a, t) result -> ('a -> 'b) -> ('b, t) result
  (** Last step: transform the value; the transform cannot fail. *)
end

val runtime : string -> ('a, t) result

val schema : string -> string -> ('a, t) result
(** [schema json_path message]. *)

val expr : string -> ('a, t) result
val unsupported : string -> ('a, t) result
val map_list : ('a -> ('b, t) result) -> 'a list -> ('b list, t) result
val pp : Format.formatter -> t -> unit
val to_string : t -> string
val pp_diagnostic : Format.formatter -> diagnostic -> unit
val diagnostic_to_string : diagnostic -> string
