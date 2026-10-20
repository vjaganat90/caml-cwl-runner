(** The data model of ccr, this CWL runner: its variant and record types and its
    effect signatures, defined once. [Error], [Untyped_tree], [Schema],
    [Command_line_tool], [Workflow], [Document], [Expr], [Glob] and [Io] each
    re-export the module of the same name below with
    [include module type of Data.X]. [Ty] ([lib/ty.ml]) does the same for
    [Type], and is [Cwl.Type] outside the library. So the comments here document
    the types those modules show. This module is private, does no I/O, and has
    one function, [Schema.Container_outdir.of_string]: a path docker could mount
    somewhere else is refused, not rewritten.

    How the parts fit. A process file is read into an [Untyped_tree.value], then
    into a [Document.t], whose [Command_line_tool.t] or [Workflow.t] is built
    from [Schema] parts and [Type]s. A job file is read into a [Type.object_],
    the fields of its tree checked against the inputs' types, and [Cwl.run]
    returns one too. [Expr] says what an expression sees and what evaluates it,
    [Glob] what globbing needs of a file system, and [Io] what code may do to
    the machine. [Error] holds the failures, and the diagnostics for constructs
    that are read and not implemented.

    A [json_path] is the dotted location of a node in a document, such as
    [inputs[0].type] or [requirements.DockerRequirement]. A top-level field is
    just its name ([class]); an error about the whole document is at [/]. *)

module Error = struct
  (** A failure: the operation cannot produce a result. The CLI prints it and
      exits 33 for [Unsupported], 1 for every other case. *)
  type t =
    | Parse of { path : string option; message : string }
        (** The text is not valid YAML or JSON. [path] is the file it came from,
            when known. [message] is the YAML parser's. *)
    | Schema of { path : string; message : string }
        (** The document breaks the CWL schema: a missing field, a value of the
            wrong shape. A job that is neither empty nor an object is the same
            error. [path] is the [json_path] of the offending node, [/] for the
            whole document, or the file for an import cycle. For an error about
            [$import], [$include], [$base] or [$graph] it is just a key name. *)
    | Unsupported of { feature : string }
        (** A CWL feature ccr does not implement was required; the CLI exits 33.
            [feature] names it: a class ([ExpressionTool]), a field
            ([outputEval]) or [remote $import]. *)
    | Type of { param : string; expected : string; got : string }
        (** A value does not have its declared type. [param] is the input or
            output id, or the path of a File or Directory output of the wrong
            kind. [expected] is the type ([int], [File[]]). [got] is what was
            found: a kind ([string], [Directory]), the offending text, or the
            phrase [not a file] or [not a directory] for a path that is neither
            a regular file nor a directory. *)
    | Expr of { message : string }
        (** An expression or parameter reference failed to evaluate: an unknown
            field, an index out of range, an unclosed [$(], a [position] that is
            not an int. *)
    | Missing of { param : string }
        (** The job gives no value, or null, for the input [param], which is not
            optional and has no default. *)
    | Runtime of { message : string }
        (** Anything else that goes wrong in a run: a file that cannot be read
            or copied, a command that cannot start or exits with a code outside
            [successCodes], a failed docker command, a required output that no
            glob finds, a path outside the allowed roots. *)

  type diagnostic = {
    feature : string;
        (** The construct: a field ([secondaryFiles]), a class
            ([InitialWorkDirRequirement]) or a prefixed form ([extension x:y],
            [unknown field f], [type T], [ResourceRequirement.ramMin]). *)
    json_path : string;  (** The [json_path] of the construct. *)
    in_requirements : bool;
        (** [true] when it was found under [requirements], where CWL makes it
            mandatory; [false] under [hints] and elsewhere, and for the unknown
            fields of a [DockerRequirement]. The flag only adds [(requirement)]
            to the text: [Cwl.run] fails on an unimplemented class through its
            [Schema.Unimplemented] entry, not through the diagnostic. *)
    message : string;
        (** [<feature> is not implemented], plus [ (requirement)] when
            [in_requirements]. [Error.pp_diagnostic] prints it after the feature
            and the [json_path]. *)
  }
  (** A construct that was read and is not implemented. It is returned beside a
      successful result (see [annotated]) instead of failing the parse. *)

  type 'a annotated = { value : 'a; diagnostics : diagnostic list }
  (** A successful result with the diagnostics found while producing it. The
      [of_tree] functions, [Cwl.command_line] and [Cwl.run] return one. The CLI
      prints the diagnostics on stderr unless it is given [--quiet]. *)
end

module Untyped_tree = struct
  (** Nested YAML or JSON; JSON is read as YAML. A process document is read from
      it into a [Document.t]; a job, a default, an expression result and an
      output are this tree too, as [Type.value]. *)
  type value =
    | Null  (** [null], [~], an empty value or an empty document. *)
    | Bool of bool
    | Int of int64
        (** A number with no fractional part in the signed 64-bit range. The
            YAML reader gives every number as a float, so [1.0] is [Int 1L] too
            and an integer above 2^53 is rounded. *)
    | Float of float
        (** Any other number: with a fraction, or outside the 64-bit range. *)
    | String of string
    | Array of value list
    | Object of (string * value) list
        (** A mapping: keys and values in document order. A repeated key is
            kept, and a lookup finds the first. *)

  (** What reading a document needs of the world: a file's content.
      [Untyped_tree.load] takes one to read a document and the files it
      [$import]s or [$include]s. Every [Io.READ] is one. *)
  module type FILE = sig
    val read_file : string -> (string, Error.t) result
    (** [read_file p] is the whole content of the file at [p]. It fails when [p]
        cannot be read; the local runtime reports [Error.Runtime]. *)
  end
end

module Type = struct
  type value = Untyped_tree.value
  (** A CWL value: an input, a default, an output or an expression result. It is
      the tree itself, so every field a document, job, expression or
      [cwl.output.json] gives is kept, in order. Its constructors are those of
      [Untyped_tree.value], written qualified ([Untyped_tree.String s]), so a
      constructor of [Type] always names a CWL type. An [Int] value is an [Int]
      or a [Long], with no range check, and a [Float] value a [Float] or a
      [Double].

      An [Object] whose [class] is ["File"] is a File, and one whose [class] is
      ["Directory"] a Directory (Process.yml, File and Directory), when the
      fields ccr reads have their types: [location], [path], and for a File also
      [basename], [nameroot], [nameext] and [checksum], are strings or absent,
      and a File's [size] is an int or absent. [location] is where the content
      is: a [file://] URI or a plain path. A relative input location is resolved
      against the directory of the document that holds it, and staging sets it
      to the copy's path as the tool sees it. [path] is the local path; after
      staging, the [basename] of the copy. Every other field ([contents],
      [format], [secondaryFiles], [listing], [dirname] and any extension) passes
      through as it is. Any other [Object] is a record, which no [t] accepts:
      [inputs] and [runtime] in expressions, or an undeclared key of
      [cwl.output.json]. *)

  (** The [position] of a [binding], its sort key on the command line: an
      integer ([Pos]), or an expression evaluated when argv is built ([Expr]; it
      must give an int, or null for 0). Bindings are ordered by ascending
      position. At equal positions the [arguments] entries come first, in order,
      then the inputs by id. *)
  type position = Pos of int | Expr of string

  type binding = {
    position : position;  (** 0 when the document gives none. *)
    prefix : string option;
    separate : bool;  (** [true] when the document gives none. *)
    item_separator : string option;
    value_from : string option;
        (** A constant string or an expression, evaluated with [self] set to the
            value being bound. Its result replaces that value. An [arguments]
            entry has no value, so this is the argument itself. *)
  }
  (** A CWL [CommandLineBinding]: how a value is written on the command line.
      The fields are the spec's, read with the spec's defaults. [shellQuote] and
      [loadContents] are not read; they are diagnostics. *)

  (** A CWL type. [Null] to [Directory], and [Any], are the types of that name.
      [Int] and [Long] accept the same values, as do [Float] and [Double]: no
      range is checked. *)
  type t =
    | Null
    | Boolean
    | Int
    | Long
    | Float
    | Double
    | String
        (** ccr also reads a type it does not implement ([record], [enum], a
            named type) as [String], with a diagnostic. *)
    | File
    | Directory
    | Array of { items : t; item_binding : binding option }
        (** [items] is the element type. [item_binding] is the [inputBinding]
            written inside the array type; it applies to each element, while the
            binding of the input itself applies to the array as a whole. *)
    | Union of t list
        (** Any one of the listed types: a type list in the document, or [T?],
            which is [Union [T; Null]]. *)
    | Any
        (** Schema Salad: "The Any type validates for any non-null value." So
            [Any] is not optional; [Any?] admits null. *)

  type object_ = (string * value) list
  (** Parameters by id, in order: the job's inputs, or the outputs of a run. *)

  type input_spec = { id : string; ty : t; default : value option }
  (** What checking a job needs of an input: its name, type and default. It is
      [Schema.input] without [input_binding] and [unimplemented]; it lives here
      because [Type] cannot see [Schema]. *)
end

module Schema = struct
  type input = {
    id : string;
        (** The name that keys the job object and follows [inputs.] in
            expressions. An id written [uri#name] or [dir/name] is cut to
            [name]. *)
    ty : Type.t;  (** The declared type ([type] is a keyword). *)
    default : Type.value option;  (** [default], read as a value of [ty]. *)
    input_binding : Type.binding option;
        (** The input's [inputBinding]; [None] when the key is absent. A null or
            empty [inputBinding] is [Some] with the default binding. *)
    unimplemented : Error.diagnostic list;
        (** Diagnostics for the fields of this input that ccr does not implement
            ([secondaryFiles], [format], extensions, unknown fields). The
            document's [annotated] result holds them too. *)
  }
  (** A CWL [CommandInputParameter]. Fields ccr does not implement are not kept;
      they become diagnostics. [label] and [doc] are accepted and dropped. *)

  (** The image source of a [DockerRequirement]: how [Runtime.docker] gets the
      image to run. *)
  type docker_image =
    | Pull of string
        (** [dockerPull]: an image name for [docker pull], and the image to run.
            A [dockerImageId] beside it is ignored. *)
    | Image_id of string
        (** [dockerImageId] alone: the image to run as is, with no [docker pull]
            first. *)
    | Load of { source : string; name : string option }
        (** [dockerLoad]: [source] is a path or an http(s) URL of an image
            archive for [docker load]. [name] is [dockerImageId], the image to
            run; without it, the name [docker load] reports. *)
    | Import of { source : string; name : string option }
        (** [dockerImport]: [source] is a path or an http(s) URL of a file
            system archive for [docker import]. A path ending in [.gz] or [.tgz]
            is gunzipped first. [name] is [dockerImageId], the tag; without it
            ccr derives one from [source]. *)
    | Dockerfile of { contents : string; tag : string option }
        (** [dockerFile]: [contents] is the text of a Dockerfile for
            [docker build]. [tag] is [dockerImageId], the image name; without it
            ccr derives one from [contents]. *)

  module Container_outdir : sig
    type t = private string
    (** A directory path inside a container, as docker would mount it: absolute,
        not [/], with no empty, [.] or [..] segment (so no trailing slash), no
        [:] (docker would read it as a [-v] separator) and no control character.
        Only [of_string] builds one; [(p :> string)] reads it. *)

    val of_string : string -> (t, string) result
    (** [of_string s] is [Ok s] when [s] meets the rules of [t]; it never
        rewrites [s]. Otherwise it is [Error m] for the first rule broken, [m]
        completing a sentence about the path: [of_string "out"] is
        [Error "must be an absolute path"]. *)
  end = struct
    type t = string

    let control c = c < ' ' || c = '\127'

    let of_string path =
      let bad message = Error message in
      if path = "" then bad "must be a non-empty string"
      else if not (String.starts_with ~prefix:"/" path) then
        bad "must be an absolute path"
      else if String.contains path ':' then bad "must not contain ':'"
      else if String.exists control path then
        bad "must not contain control characters"
      else
        let segs =
          String.split_on_char '/' path |> List.filter (fun s -> s <> "")
        in
        let canonical = "/" ^ String.concat "/" segs in
        if segs = [] then bad "must not be /"
        else if path <> canonical then
          bad "must not contain an empty path segment"
        else if List.exists (fun s -> s = "." || s = "..") segs then
          bad "must not contain . or .."
        else Ok path
  end

  type docker = {
    image : docker_image;
    output_directory : Container_outdir.t option;
        (** [dockerOutputDirectory]: the container path where the host output
            directory is mounted. It is also the tool's working directory and
            its [runtime.outdir]. [None]: the host path itself is used as that
            path. *)
  }
  (** A [DockerRequirement] that ccr can run. *)

  (** An entry of [requirements] or [hints], by class. Under [hints] a [Docker]
      is parsed and reported but not used: the run stays local. *)
  type requirement =
    | Resource of { cores_min : float option }
        (** A [ResourceRequirement]. Only [coresMin] is read, and only as a
            literal number: an expression, or no value, gives [None]. Its other
            fields are diagnostics. *)
    | Docker of docker
    | Unimplemented of { class_ : string; in_requirements : bool }
        (** A requirement class ccr does not implement; [class_] is its name as
            written. [in_requirements] is [true] under [requirements] and
            [false] under [hints]. [Cwl.run] fails with [Error.Unsupported] for
            the first one that has it [true]. *)

  type output_binding = {
    glob : string list;
        (** The [glob] patterns, each of which may hold a parameter reference. A
            single string is one pattern; an absent or null [glob] is none. *)
    unimplemented : Error.diagnostic list;
        (** Diagnostics for the other fields of the binding. When there is no
            [cwl.output.json], [Cwl.run] fails with [Error.Unsupported] if this
            holds [outputEval] or [loadContents] and [glob] is not empty, unless
            the output is of type [stdout] or [stderr]. *)
  }
  (** A CWL [CommandOutputBinding]: how to find an output among the files the
      tool wrote. *)

  (** Which stream of the tool an output captures: [Stdout] or [Stderr] for an
      output of type [stdout] or [stderr], [No_stream] for any other. *)
  type stream = Stdout | Stderr | No_stream

  type output = {
    id : string;  (** As for [input]. *)
    ty : Type.t;  (** The declared type; [File] for [stdout] and [stderr]. *)
    output_binding : output_binding option;
        (** The output's [outputBinding]; [None] when the key is absent. A null
            one is [Some] with no [glob]. *)
    stream : stream;
    unimplemented : Error.diagnostic list;  (** As for [input]. *)
  }
  (** A CWL [CommandOutputParameter]. As for [input], fields ccr does not
      implement become diagnostics, and [label] and [doc] are dropped. *)
end

module Command_line_tool = struct
  type t = {
    cwl_version : string;
        (** [cwlVersion] as written: [v1.0], [v1.1] or [v1.2], and [v1.2] when
            absent. Any other value is a schema error. All three are read the
            same way. *)
    base_command : string list;
        (** [baseCommand], a single string being a one-element list; empty when
            absent. *)
    arguments : Type.binding list;
        (** [arguments]. An entry has no input value, so its [value_from] gives
            the argument. A string entry is read as the binding whose
            [value_from] is that string: a literal, or an expression whose
            result is the argument. *)
    inputs : Schema.input list;
    outputs : Schema.output list;
    stdout : string option;
        (** [stdout]: the name, one path segment, of the file in the output
            directory that captures the standard output; an expression is
            allowed. [None]: the stream is saved as [cwl.stdout] if an output
            has type [stdout], and otherwise goes to the console. *)
    stdin : string option;
        (** [stdin]: the name, one path segment, of the file in the output
            directory that feeds the standard input; an expression is allowed.
            [None]: no input. *)
    stderr : string option;
        (** As [stdout], for the standard error and [cwl.stderr]. *)
    success_codes : int list;
        (** The exit codes that mean success; [[0]] when the document gives
            none. *)
    requirements : Schema.requirement list;
        (** What the tool needs. An unimplemented class here makes [Cwl.run]
            fail with [Error.Unsupported]. *)
    hints : Schema.requirement list;
        (** Like [requirements], but an unimplemented class is only reported. *)
  }
  (** A CWL CommandLineTool as ccr reads it. Lists keep document order. Fields
      ccr does not implement are not kept; they become diagnostics. [id],
      [label], [doc], [$namespaces], [$schemas] and [$base] are accepted and
      dropped. *)
end

module Workflow = struct
  type t = {
    cwl_version : string;
    inputs : Schema.input list;
    outputs : Schema.output list;
    requirements : Schema.requirement list;
    hints : Schema.requirement list;
  }
  (** A CWL Workflow as ccr reads it: the fields every process has, and no
      steps. Graph execution is not implemented, so [Workflow.of_tree] reports
      the class as a diagnostic and [Cwl.run] refuses it with
      [Error.Unsupported]. Its other fields, [steps] included, are dropped
      without a diagnostic of their own. *)
end

module Document = struct
  (** A process file, by its [class]. A missing [class] reads as
      CommandLineTool; any other class, such as ExpressionTool, is
      [Error.Unsupported] and not a [t]. *)
  type t = Command_line_tool of Command_line_tool.t | Workflow of Workflow.t
end

module Expr = struct
  type runtime = {
    outdir : string;
        (** [runtime.outdir]: the output directory, as the tool sees it. *)
    tmpdir : string;
        (** [runtime.tmpdir]: the temporary directory, as the tool sees it. *)
    cores : float;
        (** [runtime.cores]: [coresMin] of the first [ResourceRequirement] that
            has a numeric one, [requirements] before [hints], else 1. *)
    ram : float;
        (** [runtime.ram], in mebibytes. ccr always sets 256, the spec's
            default: [ramMin] is not read. *)
  }
  (** The [runtime] object of expressions (invocation.md, Runtime environment),
      without [outdirSize] and [tmpdirSize]. [Cwl.command_line] starts no
      process, so it fills [outdir] and [tmpdir] with placeholders. *)

  type context = { inputs : Type.object_; self : Type.value; runtime : runtime }
  (** What an expression sees. [inputs] is the job's inputs by id, with defaults
      applied (and Files staged, in a run). [self] is the value of the input
      being bound; in a [valueFrom] it is the value being bound, an element for
      an array item. It is [Null] where no input is involved, as for a file
      name, a glob or an [arguments] entry. *)

  (** An expression evaluator. [Expr.Param_ref] handles parameter references;
      inline JavaScript would be a second engine with this signature.
      [Bind.argv] takes one as a module argument. *)
  module type ENGINE = sig
    val eval : context -> string -> (Type.value, Error.t) result
    (** [eval ctx s] evaluates [s] as CWL does in a field that allows
        expressions, such as [$(inputs.n)]. A string that is exactly one
        reference gives its value with its type. A string with text around a
        reference, or with several, is interpolated into a [String]. A string
        with none is returned as a [String]. A failed lookup or an unclosed
        reference is [Error.Expr]; syntax the engine does not implement is
        [Error.Unsupported]. *)
  end
end

module Glob = struct
  (** The four questions a glob asks of a file system. [Glob.glob] takes one, so
      a test can hand it an in-memory tree. Every [Io.READ] is one. *)
  module type FS = sig
    val exists : string -> bool
    (** [exists p] is true when [p] is there, following symlinks: a dangling
        symlink is not. *)

    val is_dir : string -> bool
    (** [is_dir p] is true when [p] is a directory, or a symlink to one. *)

    val read_dir : string -> (string list, Error.t) result
    (** [read_dir p] is the names, not paths, of the entries of directory [p]:
        [read_dir "/out"] can be [Ok ["a.txt"; "sub"]]. It fails when [p] is not
        a readable directory. *)

    val realpath : string -> (string, Error.t) result
    (** [realpath p] is the absolute path of [p] with every symlink resolved. It
        fails when [p] does not exist. [Glob] uses it to keep a walk inside its
        roots. *)
  end
end

module Io = struct
  type node = [ `Not_found | `File | `Directory | `Symlink | `Other ]
  (** What a path is on disk. [`Not_found]: nothing is there. [`File]: a regular
      file. [`Directory]. [`Symlink]: a symbolic link, which only [READ.lstat]
      reports. [`Other]: a device, socket or fifo, or a path that exists but
      cannot be inspected. *)

  type stdio = {
    stdin_file : string option;
        (** The file the tool reads as standard input. [None]: an empty input
            ([/dev/null]). *)
    stdout_file : string option;
        (** The file the tool's standard output is written to, created or
            truncated. [None]: the runtime's console, by default the runner's
            stderr. *)
    stderr_file : string option;  (** As [stdout_file]. *)
  }
  (** Where the tool's three standard streams come from and go to. *)

  type tool_env = { home : string; tmpdir : string }
  (** The variables CWL defines for the tool: [HOME] is the designated outdir
      and [TMPDIR] the designated tmpdir. Paths are as the tool sees them
      (container paths under Docker). *)

  (** Effects. What a function may do to the world is the signature of the
      module it is given: [Untyped_tree.FILE] reads documents, [Glob.FS] answers
      the questions a glob asks, [READ] looks at files, [WRITE] changes them,
      [SPAWN] starts the tool, [RUNTIME] is all three, and [Expr.ENGINE]
      evaluates expressions. Outside [Runtime] itself, a function with no module
      argument does none of it. [Runtime.local] and [Runtime.docker] implement
      [RUNTIME]. *)

  (** Looking at files. It includes [Glob.FS] and [Untyped_tree.FILE], and adds
      the members below. Paths are as the runner sees them, and a relative one
      is relative to the runner's current directory. *)
  module type READ = sig
    include Glob.FS
    include Untyped_tree.FILE

    val abspath : string -> (string, Error.t) result
    (** [abspath p] is [p] spelled as a host path, with no symlink resolved
        ([realpath] does that). The local runtime returns an absolute [p] as it
        is. It does not make a relative [p] absolute: [out] gives [./out], and
        [.] and [../x] are unchanged. It fails with [Error.Runtime] only if the
        runtime has no host path for [p]. *)

    val file_size : string -> (int64, Error.t) result
    (** [file_size p] is the size in bytes of the file at [p]; a symlink is
        followed. It fails when [p] cannot be inspected. *)

    val sha1 : string -> (string, Error.t) result
    (** [sha1 p] is the lowercase hex SHA-1 of the content of the file at [p],
        read in chunks. It fails when [p] cannot be read. *)

    val lstat : string -> node
    (** [lstat p] is what [p] is itself: a symlink is [`Symlink], whatever it
        points to. *)

    val stat : string -> node
    (** [stat p] is what [p] is after following symlinks, so it is never
        [`Symlink]; a dangling link is [`Not_found]. *)

    val confined : string list -> string -> (unit, Error.t) result
    (** [confined roots p] is [Ok ()] when the real path of [p] is one of the
        directories [roots], or inside one; the roots are compared by real path
        too, so a symlink or [..] that leaves them is caught. It fails with
        [Error.Runtime] when [roots] is empty, when [p] cannot be resolved, or
        when no root holds it; a root that cannot be resolved is skipped. *)
  end

  (** Changing files. Paths are as for [READ]. *)
  module type WRITE = sig
    val mkdir_p : string -> (unit, Error.t) result
    (** [mkdir_p p] creates directory [p] and any missing parent (mode 0o755).
        It succeeds when directory [p] already exists. *)

    val mkdtemp : string -> (string, Error.t) result
    (** [mkdtemp prefix] creates a new empty directory (mode 0o700) in the
        temporary directory ([TMPDIR], else [/tmp]), named [prefix] followed by
        six random hex digits, and returns its path. The caller removes it. *)

    val copy_file : string -> string -> (unit, Error.t) result
    (** [copy_file src dst] writes the content of the regular file [src] to
        [dst], creating it or truncating it (mode 0o644). A symlink [src] is
        followed. It does not create parent directories. It fails with
        [Error.Runtime] when [src] is missing or is not a regular file. *)

    val write_file : string -> string -> (unit, Error.t) result
    (** [write_file p data] makes [p] hold exactly [data], creating it or
        truncating it (mode 0o644). *)

    val remove_tree : string -> (unit, Error.t) result
    (** [remove_tree p] deletes the file or directory tree at [p]. A symlink is
        unlinked, never followed. A missing [p] is [Ok ()]. *)
  end

  (** Starting the tool. *)
  module type SPAWN = sig
    val spawn :
      tool_env -> string -> stdio -> string list -> (int, Error.t) result
    (** [spawn env cwd stdio argv] runs [argv], the program and then its
        arguments with no shell, in directory [cwd], and waits for it. [env]
        gives the tool its [HOME] and [TMPDIR]; the local runtime adds the
        parent's [PATH] when set. The result is the exit code, so a tool that
        exits 3 is [Ok 3] and the caller checks [successCodes]. It fails with
        [Error.Runtime] when [argv] is empty, a [stdio] file is a symlink or
        cannot be opened, [cwd] is missing, the program cannot be started, or a
        signal kills it. *)
  end

  (** Everything [Cwl.run] needs of the machine: [READ], [WRITE] and [SPAWN]. *)
  module type RUNTIME = sig
    include READ
    include WRITE
    include SPAWN
  end
end
