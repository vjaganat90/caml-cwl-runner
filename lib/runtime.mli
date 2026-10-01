(** Stage files and spawn processes. The local body is Eio; tests pack a fake.
    Child cwd is the CWL outdir. Does not parse CWL or build argv. *)

include module type of Data.Runtime

val env_list : tool_env -> string list
(** [HOME] and [TMPDIR] as [NAME=value]. *)

type console = Eio.Flow.sink_ty Eio.Resource.t
(** Where a tool's uncaptured stdout and stderr go. The CLI default is the
    runner's stderr, so the output JSON keeps stdout to itself. *)

val local : ?console:console -> Eio_unix.Stdenv.base -> (module RUNTIME)
(** Runs the tool on the host. Its whole environment is {!env_list} plus the
    parent's [PATH] when set, which [spawn] reads. *)

val docker_executable : Eio_unix.Stdenv.base -> string
(** [docker] when it is on [PATH], else the first known install location that
    exists. *)

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
