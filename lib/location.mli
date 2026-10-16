(** Locations and host paths: reading a [location] or an import name as a path,
    resolving a relative one against a directory, and the lexical path tests the
    runner makes (is a path under a root, what is it relative to it). Follows
    the [location] field of File and Directory in Process.yml, and Schema
    Salad's resolution of a relative reference against the document that holds
    it. Pure: it reads no file and resolves no symlink, so [..] and links are
    taken as written. *)

val trim_slash : string -> string
(** [trim_slash s] is [s] without one trailing [/], so a lone [/] stays:
    ["/out/"] is ["/out"]. The roots and outdirs compared below go through it,
    so a trailing slash in a configured path does not matter. *)

val relative : string -> string -> string option
(** [relative root path] is the part of [path] below [root]: [Some ""] when
    [path] is [root] (without its trailing [/]), [Some x] when [path] is
    [root/x], and [None] otherwise, so ["/out2"] is not below ["/out"]. It is
    textual: neither path is cleaned. It turns an absolute glob into one
    relative to the outdir, and moves a container path to the host. *)

val under : string -> string -> bool
(** [under root path] is true when [path] is [root] or lies below it, after
    dropping one trailing [/] from each (see {!relative}): [/out2] is not under
    [/out], and [/etc] is not under [/]. [Glob] uses it for its roots and
    [Runtime] to confine outputs. *)

val normalize : string -> string
(** [normalize p] is [p] cleaned lexically: empty and [.] segments are dropped,
    a [..] pops the segment before it, and a [..] with nothing to pop is
    dropped. A leading [/] is kept: ["/a/./b/../c"] is ["/a/c"], and ["../x"] is
    ["x"]. *)

val to_path : string -> string
(** [to_path s] is the path a [location] or import name [s] stands for. Both are
    URI references, so a [%] followed by two hex digits is decoded to that byte
    (RFC 3986, 2.1), and any other [%] is kept: ["A%3AB"] is ["A:B"]. A
    [file://] URI also loses its scheme and host: ["file:///a"] and
    ["file://h/a"] are both ["/a"]. Anything else stays as it is, so a relative
    reference stays relative for {!locate} to resolve. *)

val to_uri : string -> string
(** [to_uri loc] is the [file://] URI of the path [loc], which is how a
    [location] is printed: ["/out/a.txt"] is ["file:///out/a.txt"]. A [loc] that
    already starts with [file:] is returned as it is. The path is not
    percent-encoded. *)

val locate : feature:string -> string -> string -> (string, Error.t) result
(** [locate ~feature dir name] is the path that the import name [name] (a
    [$import], [$include] or [$base]) refers to from the directory [dir]: a
    [file://] name is read as a path ({!to_path}), a relative one is put under
    [dir], and the result is {!normalize}d. An [http] or [https] [name] or [dir]
    is [Error.Unsupported] for [feature], since ccr fetches nothing remote. *)
