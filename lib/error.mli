(** Failure of the current operation ([t]) vs a parsed construct that is not
    implemented ([diagnostic]). [in_requirements] is fatal at execute, not at
    argv. No I/O. *)

include module type of Data.Error

val unimplemented : ?in_requirements:bool -> string -> string -> diagnostic
val ( let* ) : ('a, t) result -> ('a -> ('b, t) result) -> ('b, t) result
val map_list : ('a -> ('b, t) result) -> 'a list -> ('b list, t) result
val pp : Format.formatter -> t -> unit
val to_string : t -> string
val pp_diagnostic : Format.formatter -> diagnostic -> unit
val diagnostic_to_string : diagnostic -> string
