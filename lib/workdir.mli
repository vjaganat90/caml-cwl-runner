(** Paths of File and Directory values: staging the job's inputs for the tool,
    each in a directory of its own outside the output directory, mapping the
    tool's paths back to the host, and confining its outputs to the output
    directory and filling in their checksums.

    [outdir] is the designated output directory (the spec's term) as a host
    path; it is the tool's working directory. {!relocate} and {!resolve_paths}
    call it [host]. [container] is the path the tool sees for it: [outdir]
    itself, or the [dockerOutputDirectory] when the tool's [requirements] hold a
    [DockerRequirement] that sets one. The locations these functions produce are
    plain paths; [Ty.to_json] adds [file://] when it prints a [location]. Each
    function changes only the fields it names and keeps every other field of a
    File or Directory.

    Follows the File and Directory rules of the Process spec and "Output
    binding" in [invocation.md]: a relative output path is relative to [outdir],
    an absolute one must stay inside it, and on an output [path] beats
    [location]. (An input is the other way round: {!stage} reads [location]
    first.)

    {!locate} reads an input's location before {!stage} stages it. Only
    {!stage}, {!confine} and {!checksum} touch files, and only through the
    [Runtime] module they are given. Does not glob, spawn, or build argv. *)

val safe_leaf : what:string -> string -> (string, Error.t) result
(** [safe_leaf ~what s] is [Ok s] when [s] is a single path segment: not empty,
    not [.] or [..], and without slash, backslash or NUL. Otherwise it is an
    [Error.Runtime] whose message starts with [what]. It guards the names the
    runner joins onto a directory (staged basenames, onto their staging
    directory, and the [stdout] and [stderr] file names, onto [outdir]), so none
    can point outside it. *)

val locate : string -> Ty.value -> (Ty.value, Error.t) result
(** [locate dir v] is [v] with the [location] of each File and Directory (also
    inside arrays and records) set to the host path it names, relative to the
    directory [dir]. The source is the [location], read as a path by
    [Location.to_path] ([file://] dropped, percent escapes decoded), else the
    [path] as it is; a relative one is put under [dir], so an absolute [dir]
    gives an absolute source. A Directory's [path] becomes that source too; it
    is not checked to exist. Pass the directory of the document that holds [v]
    (Process.yml, File and Directory [location]): the job file's for a job
    value, the tool's for a [default]. A File literal, one with [contents] and
    neither [location] nor [path], is left as it is for {!stage}; so is a
    Directory literal, one with a [listing] and neither field, except that the
    entries of its [listing] are located in turn.

    Fails with [Error.Runtime] when a File or Directory that is not a literal
    has neither [location] nor [path], or its location has a scheme other than
    [file]. *)

val stage :
  (module Io.WRITE) -> string -> Ty.object_ -> (Ty.object_, Error.t) result
(** [stage (module R) root inputs] is [inputs] with every File and Directory
    (also inside arrays and records) staged in a directory of its own,
    [root/<n>/b], made here, where [n] is a number no other input gets and [b]
    is its [basename], else the last segment of its source, the host path in its
    [location] (see {!locate}, which must come first). Process.yml, File
    [basename]: "the files and secondary files may be staged to an arbitrary
    directory, but must use the value of [basename] for the filename", so two
    inputs with one basename do not meet. [root] is a host directory outside the
    designated output directory, so no output glob sees an input. A located File
    is copied there, and a located Directory is a symbolic link to its source
    (see [Io.WRITE.symlink]), not a copy. The staged value's [path] and
    [location] are the absolute host path [root/<n>/b], and [basename] is [b];
    its other fields are kept, and a missing [nameroot] and [nameext] are filled
    from [b].

    A File literal (see {!locate}) is written there instead, its [contents] as
    the file's text, and named [literal-<n>] when it has no [basename], [n]
    being the number of Files and Directories staged before it in that directory
    (Process.yml, File [contents]). A Directory literal (see {!locate}) is
    created there as a directory, named as a File literal is, and the entries of
    its [listing] are staged into it the same way, but side by side, so their
    [path] is under its own (Process.yml, Directory: "Directories must be
    recursively staged first and have local values of [path] assigned"). Any
    other File without a [location] is left as it is.

    Fails with [Error.Runtime] when a basename is not a {!safe_leaf}, two
    entries of one Directory literal share a basename, or a File or Directory
    cannot be copied, linked or written: its source is missing or, for a File,
    not a regular file. *)

val sources : Ty.object_ -> string list
(** [sources inputs] is the host path of each located Directory in [inputs],
    also one in the [listing] of a Directory literal: what {!stage} links to.
    Call it on the values {!locate} gives. *)

val export :
  (module Io.RUNTIME) ->
  string ->
  string ->
  Ty.value ->
  (Ty.value, Error.t) result
(** [export (module R) root outdir v] copies each File and Directory in [v]
    whose [path] lies under the staging root [root] (a staged input, or a File
    or Directory literal, that an [outputEval] returns) into [outdir], a
    Directory with its whole tree, and sets its [path] and [location] to the
    copy; its [listing], if any, still names the staged entries. The staging
    root is removed after the run, and invocation.md has output files "written
    under the designated output directory". The copy is named [outdir/b], [b]
    the last segment of the staged path (its [basename]), or, when that exists,
    the first free [b_2], [b_3], ...: cwltool's rule for a name clash when it
    relocates outputs. [basename] is kept. Call it after {!confine}, which gives
    real paths. Any other value is left as it is, so a located input Directory
    keeps its host path, which outlives the run.

    Fails with [Error.Runtime] when a copy fails. *)

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
  (module Io.READ) -> string list -> Ty.value -> (Ty.value, Error.t) result
(** [confine (module R) roots v] takes the [path], else the [location], of each
    File and Directory in [v], and returns the value with [path] and [location]
    both set to that path's real path (symlinks resolved). A missing [basename]
    becomes the last segment of the real path, and a File's missing [nameroot]
    and [nameext] are split from its [basename]. The path must exist, lie in or
    under one of the directories [roots], and be a regular file for a File and a
    directory for a Directory. A relative path is looked up from the runner's
    working directory, not from [roots], so call {!resolve_paths} first.

    Fails with [Error.Runtime] when a File or Directory has no path, the path
    does not exist, or it is outside every root (or [roots] is empty), and with
    [Error.Type] when a File is not a regular file or a Directory is not a
    directory. It implements the spec's rule that an output path must refer to a
    path within the output directory. *)

val checksum : (module Io.READ) -> Ty.value -> (Ty.value, Error.t) result
(** [checksum (module R) v] fills in, on each File in [v] and in the [listing]
    of each Directory in it, at any depth, whichever of [checksum] (["sha1$"]
    followed by the hex SHA-1 of the contents) and [size] (in bytes) it lacks; a
    value already present is kept, not verified. The file is read at its [path],
    else its [location], which is taken as a plain path ([file://] is not
    stripped); a File with neither is left as it is. Fails with [Error.Runtime]
    when a file that must be read cannot be (a File with both values is not
    read). The Process spec makes computing output checksums a condition of
    passing its conformance suite. *)
