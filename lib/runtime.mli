(** Stage files and spawn processes. The local body is Eio; tests pack a fake.
    Child cwd is the CWL outdir. Does not parse CWL or build argv. *)

type node = [ `Not_found | `File | `Directory | `Symlink | `Other ]

type stdio = {
  stdin_file : string option;
  stdout_file : string option;
  stderr_file : string option;
}

type tool_env = { home : string; tmpdir : string; path : string option }
(** The environment CWL gives the tool: [HOME] is the designated outdir,
    [TMPDIR] the designated tmpdir, and [PATH] the parent's when set. Paths are
    as the tool sees them (container paths under Docker). *)

module type RUNTIME = sig
  include Glob.FS

  val mkdir_p : string -> (unit, Error.t) result
  val abspath : string -> (string, Error.t) result
  val mkdtemp : string -> (string, Error.t) result
  val copy_file : string -> string -> (unit, Error.t) result
  val read_file : string -> (string, Error.t) result
  val write_file : string -> string -> (unit, Error.t) result
  val file_size : string -> (int64, Error.t) result

  val sha1 : string -> (string, Error.t) result
  (** Lowercase hex SHA-1 of the file contents, read in chunks. *)

  val remove_tree : string -> (unit, Error.t) result
  (** Delete a file or directory tree. Symlinks are unlinked, never followed. A
      missing path is [Ok ()]. *)

  val lstat : string -> node
  val stat : string -> node
  val realpath : string -> (string, Error.t) result
  val confined : string list -> string -> (unit, Error.t) result

  val spawn :
    tool_env -> string -> stdio -> string list -> (int, Error.t) result
end

val tool_env : outdir:string -> tmpdir:string -> tool_env
(** [PATH] is copied from the parent when it is set. *)

val env_list : tool_env -> string list
(** [HOME], [TMPDIR], and [PATH] when set, as [NAME=value]. A local tool's whole
    environment; no other variable is included. *)

type console = Eio.Flow.sink_ty Eio.Resource.t
(** Where a tool's uncaptured stdout and stderr go. The CLI default is the
    runner's stderr, so the output JSON keeps stdout to itself. *)

val local : ?console:console -> Eio_unix.Stdenv.base -> (module RUNTIME)
val docker_executable : unit -> string

type docker_spec = {
  bin : string;
  user : string;
  cwd : string;
  workdir : string;
  image : string;
  mounts : (string * string) list;  (** Extra [-v source:target] pairs. *)
  container_env : string list;  (** [NAME=value] set inside the container. *)
}

val docker_mount :
  host:string -> workdir:string -> (string * string, Error.t) result
(** Bind-mount pair. The source is [realpath] of the host outdir, so a symlink
    or [..] in that path cannot name a different directory than the one the
    runner reads. The target is [workdir]. Both sides must be canonical absolute
    paths ([Schema.Container_outdir]); a ':' would be a [-v] option. *)

val docker_run_argv : docker_spec -> string list -> string list
(** [docker run] argv. The host [cwd] is bind-mounted at [workdir] and [argv] is
    the suffix: the CWL command in exec form. *)

val docker :
  ?console:console -> Eio_unix.Stdenv.base -> Schema.docker -> (module RUNTIME)
(** Same filesystem as [local]. [spawn] acquires the image ([docker pull], an
    existing id, [docker load], [docker import], or [docker build]) then
    [docker run]. The host outdir is bind-mounted at [dockerOutputDirectory] and
    that path is the workdir; when the field is absent the mount target is the
    host path. The CWL argv is the exec form, not a shell string. *)
