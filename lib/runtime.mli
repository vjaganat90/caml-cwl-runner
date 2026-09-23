(** Stage files and spawn processes. The local body is Eio; tests pack a fake.
    Child cwd is the CWL outdir. Does not parse CWL or build argv. *)

type node = [ `Not_found | `File | `Directory | `Symlink | `Other ]

type stdio = {
  stdin_file : string option;
  stdout_file : string option;
  stderr_file : string option;
}

module type RUNTIME = sig
  include Glob.FS

  val mkdir_p : string -> (unit, Error.t) result
  val abspath : string -> (string, Error.t) result
  val mkdtemp : string -> (string, Error.t) result
  val copy_file : string -> string -> (unit, Error.t) result
  val read_file : string -> (string, Error.t) result
  val write_file : string -> string -> (unit, Error.t) result
  val file_size : string -> (int64, Error.t) result
  val lstat : string -> node
  val stat : string -> node
  val realpath : string -> (string, Error.t) result
  val confined : string list -> string -> (unit, Error.t) result
  val spawn : string -> stdio -> string list -> (int, Error.t) result
end

val local : Eio_unix.Stdenv.base -> (module RUNTIME)
val docker_executable : unit -> string

type docker_spec = { bin : string; user : string; cwd : string; image : string }

val docker_run_argv : docker_spec -> string list -> string list
(** [docker run] argv. [argv] is the suffix: the CWL command in exec form. *)

val docker : Eio_unix.Stdenv.base -> Schema.docker_image -> (module RUNTIME)
(** Same filesystem as [local]. [spawn] acquires the image ([docker pull], an
    existing id, [docker load], [docker import], or [docker build]) then [docker
    run]. The host outdir is bind-mounted as the workdir. The CWL argv is the
    exec form, not a shell string. *)
