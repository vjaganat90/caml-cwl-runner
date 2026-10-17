(** The two runtimes: {!local} starts the tool as a child process of the runner,
    and {!docker} starts it in a container. They share one set of file
    operations on the host and differ only in how [spawn] launches the tool. Eio
    does the work, so apart from the pure {!env_list} and {!docker_run_argv},
    call these inside [Eio_main.run].

    [READ] (inspect files), [WRITE] (change them), [SPAWN] (start the tool) and
    [RUNTIME] (all three) are declared in {!Cwl.Io}, where their members are
    documented. A function takes the smallest one that covers what it does, so
    its type says what it may touch.

    Follows "Runtime environment" in [invocation.md]: the tool starts in the
    designated output directory (the [cwd] given to [spawn]) and, under
    {!local}, sees only [HOME], [TMPDIR] and [PATH]. {!docker} implements
    [DockerRequirement]. Failures are [Error.Runtime]; Eio cancellation is
    re-raised. Does not parse CWL or build the tool's own command line;
    {!docker_run_argv} only wraps it. *)

val env_list : tool_env -> string list
(** [env_list e] is the two strings [HOME=h] and [TMPDIR=t], in that order,
    where [h] is [e.home] and [t] is [e.tmpdir]. They are the variables the spec
    requires of every tool; {!local} gives the tool these two plus [PATH]. *)

type console = Eio.Flow.sink_ty Eio.Resource.t
(** Where output goes when no file captures it: the tool's stdout and stderr,
    and the stderr of the helper commands that {!docker} runs to get the image.
    The default is the runner's stderr, which the CLI uses to leave its stdout
    to the output JSON. *)

val local : ?console:console -> Eio_unix.Stdenv.base -> (module RUNTIME)
(** [local env] runs the tool as a child process of the runner. Its whole
    environment is {!env_list} plus the runner's [PATH], if it has one, read
    each time [spawn] starts a tool. File operations act on the host, and a
    relative path is relative to the runner's working directory. [console]
    defaults to the runner's stderr. *)

val docker_executable : Eio_unix.Stdenv.base -> string
(** [docker_executable env] is ["docker"] when a file of that name is in a
    directory of the runner's [PATH]. Otherwise it is the first of
    [/Applications/Docker.app/Contents/Resources/bin/docker],
    [/opt/homebrew/bin/docker] and [/usr/local/bin/docker] that is a file, so
    Docker is found when [PATH] lacks it; if none is, it is ["docker"]. *)

type docker_spec = {
  bin : string;
      (** The Docker client: a name found on [PATH] or an absolute path. *)
  user : string;  (** [uid:gid] the container runs as ([--user]). *)
  cwd : string;  (** Host directory bind-mounted at [workdir]. *)
  workdir : string;
      (** Container path of that mount, and the working directory ([-w]). *)
  tag : string;  (** Image to run. *)
  mounts : (string * string) list;  (** Extra [-v source:target] pairs. *)
  container_env : string list;  (** [NAME=value] set inside the container. *)
}
(** The parts of one [docker run] command line, put together by
    {!docker_run_argv}. *)

val docker_mount :
  host:string -> workdir:string -> (string * string, Error.t) result
(** [docker_mount ~host ~workdir] is the pair [(source, target)] of a
    [-v source:target] bind mount. [source] is the [realpath] of the host
    directory [host], so a symlink or [..] in [host] cannot name a different
    directory than the one the runner reads; [target] is [workdir]. Fails with
    [Error.Runtime] when [host] cannot be resolved, or when either side is not a
    canonical absolute path: one that is not [/], has no empty, [.] or [..]
    segment (so no trailing slash), and contains no [:] (Docker would read it as
    a [-v] separator) or control character. *)

val docker_run_argv : docker_spec -> string list -> string list
(** [docker_run_argv spec argv] is the command line of the Docker client:
    [spec.bin run --rm --user spec.user], the [-v] mounts ([spec.cwd] at
    [spec.workdir], then [spec.mounts]), a [--env] for each of
    [spec.container_env], [-w spec.workdir], the image [spec.tag], and [argv]
    last. As [argv] follows the image name, Docker runs it as the container's
    command (or, if the image has an [ENTRYPOINT], passes it as that program's
    arguments) with no shell in between (exec form). There is no [-i], so the
    container's stdin is not connected to the client's. *)

val docker :
  ?console:console -> Eio_unix.Stdenv.base -> Schema.docker -> (module RUNTIME)
(** [docker env req] has the file operations of {!local}, but [spawn] runs the
    tool with [docker run]. Each [spawn] first acquires the image [req.image]
    ([docker pull], an existing image id, [docker load], [docker import] or
    [docker build]), then runs {!docker_run_argv} as the runner's user. The
    [cwd] given to [spawn] is bind-mounted, by its [realpath], at
    [req.output_directory] ([dockerOutputDirectory]) or, when that is absent, at
    the [cwd] path itself; the mount is the container's working directory. The
    tmpdir is mounted at its own path. Nothing else of the host is mounted, so a
    host path outside those two, such as an input Directory, is not visible in
    the container. [HOME] and [TMPDIR] come from the [tool_env] given to
    [spawn]; the Docker client itself runs with the runner's environment. The
    container gets no standard input: [stdin_file] is opened for the client only
    (see {!docker_run_argv}). [spawn] returns the exit code of the [docker run]
    client, and fails with [Error.Runtime] when a [docker] command fails or a
    mount path is not canonical. [console] defaults to the runner's stderr. *)
