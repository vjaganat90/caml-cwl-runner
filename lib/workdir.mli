(** File and Directory values against a work directory. Stages inputs, relocates
    container paths, confines outputs, and fills checksums. Does not glob,
    spawn, or build argv. *)

val safe_leaf : what:string -> string -> (string, Error.t) result
(** A single path segment: not empty, not [.] or [..], no slash or NUL. *)

val stage :
  (module Runtime.WRITE) ->
  container:string ->
  outdir:string ->
  job_dir:string ->
  Ty.object_ ->
  (Ty.object_, Error.t) result
(** Copy each File into [outdir] under its basename and record the container
    path. A Directory keeps its resolved source path. [job_dir] resolves a
    relative location. *)

val relocate : container:string -> host:string -> string -> string
(** A path the tool saw under [container], as a path on [host]. A path outside
    [container] is unchanged. *)

val resolve_paths : container:string -> string -> Ty.value -> Ty.value
(** [resolve_paths container host v]. [file://] and paths under [container]
    become paths on [host]; a relative path is under [host]. *)

val confine :
  (module Runtime.READ) -> string list -> Ty.value -> (Ty.value, Error.t) result
(** Each File and Directory path must sit under one of the roots, and the node
    kind must match the value. *)

val checksum : (module Runtime.READ) -> Ty.value -> (Ty.value, Error.t) result
(** Fill a missing [checksum] and [size] on each File. *)
