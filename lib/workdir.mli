(** Paths of File and Directory values: staging the job's input files for the
    tool, mapping the tool's paths back to the host, and confining its outputs
    to the output directory and filling in their checksums.

    [outdir] is the designated output directory (the spec's term) as a host
    path; it is the tool's working directory. {!relocate} and {!resolve_paths}
    call it [host]. [container] is the path the tool sees for it: [outdir]
    itself, or the [dockerOutputDirectory] when the tool's [requirements] hold a
    [DockerRequirement] that sets one. The locations these functions produce are
    plain paths; [Ty.to_json] adds [file://] when it prints a [location].

    Follows the File and Directory rules of the Process spec and "Output
    binding" in [invocation.md]: a relative output path is relative to [outdir],
    an absolute one must stay inside it, and on an output [path] beats
    [location]. (An input is the other way round: {!stage} reads [location]
    first.)

    {!locate} reads an input's location before {!stage} copies it. Only
    {!stage}, {!confine} and {!checksum} touch files, and only through the
    [Runtime] module they are given. Does not glob, spawn, or build argv. *)

val safe_leaf : what:string -> string -> (string, Error.t) result
(** [safe_leaf ~what s] is [Ok s] when [s] is a single path segment: not empty,
    not [.] or [..], and without slash, backslash or NUL. Otherwise it is an
    [Error.Runtime] whose message starts with [what]. It guards the names the
    runner joins onto [outdir] (File basenames and the [stdin], [stdout] and
    [stderr] file names), so none can point outside it. *)

val locate : string -> Ty.value -> (Ty.value, Error.t) result
(** [locate dir v] is [v] with the [location] of each File and Directory (also
    inside arrays and records) set to the host path it names, relative to the
    directory [dir]. The source is the [location], read as a path by
    [Location.to_path] ([file://] dropped, percent escapes decoded), else the
    [path] as it is; a relative one is put under [dir], so an absolute [dir]
    gives an absolute source. A Directory's [path] becomes that source too; it
    is not checked to exist. Pass the directory of the document that holds [v]
    (Process.yml, File and Directory [location]): the job file's for a job
    value, the tool's for a [default].

    Fails with [Error.Runtime] when a File or Directory has neither [location]
    nor [path], or its location has a scheme other than [file]. *)

val stage :
  (module Runtime.WRITE) ->
  container:string ->
  outdir:string ->
  Ty.object_ ->
  (Ty.object_, Error.t) result
(** [stage (module R) ~container ~outdir inputs] is [inputs] with every File
    (also inside arrays and records) copied to [outdir/b], where [b] is its
    [basename], else the last segment of its source, the host path in its
    [location] (see {!locate}, which must come first). The copy's [path] and
    [basename] are [b], a bare name that resolves from the tool's working
    directory, and its [location] is [container/b]; its other fields are kept. A
    File without a [location] and a Directory are left as they are.

    Fails with [Error.Runtime] when a basename is not a {!safe_leaf}, two Files
    share a basename (even when they are the same file), or a File cannot be
    copied: its source is missing or not a regular file, or [outdir] does not
    exist. *)

val relocate : container:string -> host:string -> string -> string
(** [relocate ~container ~host p] moves [p] from under [container] to under
    [host]: [p = container] gives [host], and [container/x] gives [host/x], with
    [.] and empty segments of [x] dropped. [p] is returned as it is when it is
    relative, has a [..] segment, lies outside [container], or when
    [container = host]. For example, with [~container:"/c" ~host:"/h"], ["/c/a"]
    becomes ["/h/a"]. [Cwl.run] uses it on glob patterns: one built from
    [$(runtime.outdir)] names a [container] path. *)

val resolve_paths : container:string -> string -> Ty.value -> Ty.value
(** [resolve_paths ~container host v] rewrites the [path] and [location] of each
    File and Directory in [v] as paths on the host, where [host] is the host
    [outdir]. The new [path] comes from the old [path], else the old [location];
    the new [location] from the old [location], read by [Location.to_path], else
    the new [path]. Both move from under [container] to under [host] (see
    {!relocate}), and, if still relative, are put under [host]. A value with
    neither field is left as it is. It reads no files and does not check that
    the result is inside [host]; {!confine} does that.

    [Cwl.run] applies it to the objects read from [cwl.output.json]: the spec
    makes a relative path there relative to the output directory, and a tool in
    a container writes [container] paths. Reading a File or Directory
    ([Ty.object_of_tree]) sets a missing [path] to the last segment of its
    [location], so one read with only the [location] [sub/x.txt] comes out with
    the [path] [host/x.txt] and the [location] [host/sub/x.txt], and {!confine}
    takes the [path]. *)

val confine :
  (module Runtime.READ) -> string list -> Ty.value -> (Ty.value, Error.t) result
(** [confine (module R) roots v] takes the [path], else the [location], of each
    File and Directory in [v], and returns the value with [path] and [location]
    both set to that path's real path (symlinks resolved). A File's missing
    [basename] becomes the last segment of the real path, and its missing
    [nameroot] and [nameext] are split from its [basename]. The path must exist,
    lie in or under one of the directories [roots], and be a regular file for a
    File and a directory for a Directory. A relative path is looked up from the
    runner's working directory, not from [roots], so call {!resolve_paths}
    first.

    Fails with [Error.Runtime] when a File or Directory has no path, the path
    does not exist, or it is outside every root (or [roots] is empty), and with
    [Error.Type] when a File is not a regular file or a Directory is not a
    directory. It implements the spec's rule that an output path must refer to a
    path within the output directory. *)

val checksum : (module Runtime.READ) -> Ty.value -> (Ty.value, Error.t) result
(** [checksum (module R) v] fills in, on each File in [v], whichever of
    [checksum] (["sha1$"] followed by the hex SHA-1 of the contents) and [size]
    (in bytes) it lacks; a value already present is kept, not verified. The file
    is read at its [path], else its [location], which is taken as a plain path
    ([file://] is not stripped); a File with neither is left as it is. Fails
    with [Error.Runtime] when a file that must be read cannot be (a File with
    both values is not read). The Process spec makes computing output checksums
    a condition of passing its conformance suite. *)
