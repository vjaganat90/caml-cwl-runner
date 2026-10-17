(** The effect signatures [READ], [WRITE], [SPAWN] and [RUNTIME], and the types
    they use. They are the whole of what the pure modules may do to the machine;
    the [Runtime] library implements them. See [Data.Io] for the members. *)

include Data.Io
