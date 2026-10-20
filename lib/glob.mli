(** The paths an [outputBinding.glob] names. Implements the [glob] field of
    CommandOutputBinding in CommandLineTool.yml: matching in the style of POSIX
    glob(3), relative to the output directory, with [**] added, and a refusal to
    look outside that directory. It walks a filesystem given as an [FS], so
    tests can use an in-memory one. It reads no file contents, spawns nothing
    and does not call Eio.

    Pattern. A pattern is split at [/] into components; empty ones are dropped,
    so a trailing [/] counts for nothing. Wildcards are those of [Re.Glob] (the
    [re] library): in a component [*] matches any run of bytes, [?] one byte (a
    non-ASCII character takes several) and [[a-z]] a set (negated by [!] or
    [^]); none matches [/] or a leading [.]. A component that is just [**]
    stands for zero or more directory levels, dot names included; as the last
    component it matches that directory and everything below it. Braces are not
    expanded. A backslash is dropped and the character after it kept, so it does
    not make a wildcard literal: [a\*b] matches what [a*b] does.

    Roots. The [roots] of [glob] are the directories the walk may enter: it
    enters a directory only if its [realpath] is under one of them, so a symlink
    cannot lead it outside the roots. The paths it returns are not checked: a
    symlink is found by its own name, wherever it points. [Cwl.run] passes the
    output directory, the temporary directory and every Directory input as
    roots, then confines each path found to them. *)

include module type of Data.Glob
(** [FS] is the filesystem a walk reads. [exists] and [is_dir] look through
    symlinks, so a dangling link does not exist. [read_dir d] is the entry names
    of [d], not paths; [.] and [..] are ignored if listed. [realpath p] is the
    absolute path of [p] with symlinks resolved, or an [Error]. [Io.READ]
    includes it, so every runtime is an [FS]. *)

val max_glob_depth : int
(** How many directory levels a walk goes down before it fails with
    [Error.Runtime]: a deeper tree is taken for a symlink loop. {!glob} and the
    listing of an output Directory both stop there. *)

val glob :
  (module FS) ->
  ?roots:string list ->
  string ->
  string ->
  (string list, Error.t) result
(** [glob fs ?roots root pattern] is the paths in the directory [root] that
    match [pattern], sorted by bytes and without duplicates. Each is [root]
    joined to the matching path, not resolved. [pattern] is trimmed, then taken
    relative to [root]; an absolute [pattern] must be [root] or below it, and
    [.] or an empty one is [root] itself. A path found must exist, so a dangling
    symlink is not found.

    [roots] default to [[root]], also when empty, and replace that default
    otherwise, so list [root] too. Each is resolved with [FS.realpath], or kept
    as given if that fails. A directory whose real path is outside them, or
    cannot be resolved, is not entered: nothing below it is found, and that is
    not an error.

    Fails with [Error.Runtime] for a [..] component, an absolute [pattern]
    outside [root], a walk more than 64 levels deep, and a malformed component
    (an unclosed set) if the walk gets to it. An error of [FS.read_dir] is
    returned as it is. *)
