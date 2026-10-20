(** The library's public surface: {!run} executes a CommandLineTool and
    {!command_line} only builds its argv. The other modules of the library are
    re-exported below: [Error] (failures, and the diagnostics that report a
    construct read but not implemented), [Untyped_tree] (the document reader),
    [Document], [Command_line_tool], [Workflow] and [Schema] (the typed process
    documents and their shared fields), [Type] (types and values; the library's
    [Ty]), [Expr] (parameter references), [Bind] (argv), [Glob] (output globs),
    [Location] (locations as paths), [Workdir] (File and Directory paths) and
    [Runtime] (the local and Docker runtimes).

    [run] follows [invocation.md]: stage the inputs into the output directory,
    build the command line, start the tool there with the prescribed
    environment, then collect the outputs from [cwl.output.json] or from each
    output's [glob] or captured stream. Staging, relocation, confinement and
    checksums are in [Workdir].

    Does not run Workflows ([Error.Unsupported] for the feature ["Workflow"])
    and has no JavaScript engine: an expression that is not a parameter
    reference is [Error.Unsupported] for the feature
    ["InlineJavascriptRequirement"]. *)

module Error : module type of Error
module Untyped_tree : module type of Untyped_tree
module Schema : module type of Schema
module Command_line_tool : module type of Command_line_tool
module Workflow : module type of Workflow
module Document : module type of Document
module Type : module type of Ty
module Expr : module type of Expr
module Bind : module type of Bind
module Glob : module type of Glob
module Location : module type of Location
module Workdir : module type of Workdir
module Io : module type of Io

val command_line :
  (module Untyped_tree.FILE) ->
  string ->
  string ->
  (string list Error.annotated, Error.t) result
(** [command_line (module F) tool job] reads the process document at [tool] and
    the job object at [job] through [F], and returns the command line the tool
    would run with, plus the document's diagnostics. [tool] is split at its last
    [#]: when both parts are non-empty, the second is the id of the process to
    pick from a [$graph]; without an id, the one with id [main] is used. Nothing
    is staged or run, so [runtime.outdir] and [runtime.tmpdir] are the
    placeholders [/unused-outdir] and [/unused-tmpdir], and an unimplemented
    requirement is only a diagnostic here, where {!run} fails on it.

    Fails with the [Error.t] of [F] for an unreadable file, [Parse] for text
    that is not YAML or JSON, [Schema] for a document or job that breaks the CWL
    schema, [Unsupported] for a Workflow or other process class, a remote
    [$import] or [$include], or a JavaScript expression, [Missing] or [Type]
    when the job does not fit the inputs, and [Expr] for a bad parameter
    reference. *)

val run :
  (module Io.RUNTIME) ->
  ?docker:(network:bool -> Schema.docker -> (module Io.RUNTIME)) ->
  ?outdir:string ->
  ?rm_tmpdir:bool ->
  string ->
  string option ->
  (Type.object_ Error.annotated, Error.t) result
(** [run (module Local) ?docker ?outdir ?rm_tmpdir tool job] executes the
    CommandLineTool at [tool] (a [#id] picks a process as in {!command_line}) on
    the job object at [job], and returns its output object with the document's
    diagnostics. [Local] reads the two documents. It also does the rest of the
    file work and starts the tool, unless the tool's [requirements] (not its
    [hints]) hold a [DockerRequirement]: then the runtime that [docker] returns
    for it does, called with [~network] true only when a [NetworkAccess] in
    [requirements] (else [hints]) is [true]; otherwise the container has no
    network. A local run cannot restrict the network, so it ignores
    [NetworkAccess]. Without [docker] the run fails with [Error.Runtime]. With
    [job = None] the input object is empty. A relative location in the job is
    relative to the directory of [job], and one in a [default] to that of
    [tool]; either becomes an absolute path, so the tool finds it from its own
    working directory.

    [outdir] is the designated output directory, created if missing; by default
    it is a fresh directory. A relative [outdir] is not made absolute, so [HOME]
    and [runtime.outdir] are relative too, unless a [dockerOutputDirectory]
    gives the tool another path for it. [outdir] is kept, since it holds the
    outputs. The tool's [TMPDIR] is a fresh directory, deleted when the run
    ends, whether it succeeded or not, unless [rm_tmpdir] is [false].

    If [outdir] holds a [cwl.output.json], it is the output object:

    - [outputBinding] is ignored.
    - Each declared output is type-checked. A missing one is null if optional,
      an empty array if an array, else an error.
    - Keys the tool does not declare are kept.
    - A relative path of a File or Directory is taken relative to [outdir], one
      under a [dockerOutputDirectory] is mapped back to [outdir], and every such
      path must resolve inside [outdir].

    Otherwise each declared output is read from its [glob], or, for a [stdout]
    or [stderr] output, from the file that captured that stream ([cwl.stdout] or
    [cwl.stderr] unless the tool names one):

    - A type that is an array takes every match. Any other type, an optional
      array included, takes the one match, or null when there is none and the
      type is optional; several matches are an error.
    - A match is a File, or a Directory when it is one. The type is checked only
      to refuse a Directory for a declared File and a File for a declared
      Directory.
    - A match must resolve, symbolic links followed, into [outdir], the tool's
      [TMPDIR] or an input Directory.
    - A Directory match gets its [listing]: every entry, dot names included,
      sorted by name, each subdirectory listed in turn. Each entry must resolve
      into the same directories, and a tree more than 64 levels deep is an
      error.

    Every File in the result, also inside a [listing], has its [checksum] and
    [size].

    Fails with the errors of {!command_line} for the documents, the job and the
    argv, with [Error.Parse] or [Error.Schema] for a malformed
    [cwl.output.json], and also with:

    - [Error.Unsupported] (the CLI then exits 33) for a requirement that is not
      implemented or, when there is no [cwl.output.json], for an [outputEval] or
      [loadContents] on an output that has a [glob].
    - [Error.Expr] for a bad parameter reference in a glob or in a [stdin],
      [stdout] or [stderr] name.
    - [Error.Type] when an output in [cwl.output.json] is not of its declared
      type, and when a Directory is found for a declared File or the reverse.
    - [Error.Runtime] for the rest, such as a failed file or Docker operation,
      an exit code outside [successCodes], or an output that is missing, matched
      more than once, or outside the directories above. *)
