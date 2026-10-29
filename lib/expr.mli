(** Expression strings of a CWL document: parameter references and string
    interpolation, without JavaScript. Implements Parameter references and
    String interpolation in concepts.md, including its escape rules. Does not
    run JavaScript: [${...}], and any [$(...)] that is not a plain reference, is
    [Error.Unsupported] for [InlineJavascriptRequirement]. Pure: reads no file,
    and the directories a [runtime] names need not exist. *)

include module type of Data.Expr
(** A [context] is the root of every reference: [inputs], [self] (a value the
    spec defines per field, null where it defines none) and [runtime]. A
    [runtime] has [outdir], [tmpdir], [cores] and [ram] (in MiB); the spec's
    [outdirSize] and [tmpdirSize] are not there. A reference reads a whole
    [cores] or [ram] as an int. An [ENGINE] turns the text of a field into a
    value or an [Error.t]: [eval ctx s]. [Param_ref] is the only one; a
    JavaScript engine would be another, with the same signature. *)

val default_runtime : runtime
(** [outdir] and [tmpdir] are the placeholder paths [/unused-outdir] and
    [/unused-tmpdir]: nothing creates or reads them, so use this runtime when
    only argv is built or an expression is tested. [cores] is 1 and [ram] is
    256, the spec's default [ramMin]. *)

val runtime_with_cores : float -> runtime
(** [runtime_with_cores c] is [default_runtime] with [cores = c]. It is for
    [Cwl.command_line], which makes no directories, so [$(runtime.outdir)] in
    its argv is the placeholder. *)

val runtime_with : outdir:string -> tmpdir:string -> cores:float -> runtime
(** The [runtime] of a run: the output and temporary directories the tool sees
    and the cores reserved ([Cwl.run] passes a literal [coresMin], else 1).
    [ram] is always 256: [ramMin] is not implemented and only produces a
    diagnostic. *)

module Param_ref : ENGINE
(** Evaluates parameter references; the only [ENGINE]. [eval ctx s] is:
    - the referenced value, keeping its type, when [s] is one reference with at
      most whitespace around it: [$(inputs.n)] is the int 3 when [n] is 3;
    - else, if [s] has a [$] or a backslash, a string in which each reference is
      replaced by its text, so [n=$(inputs.n)] is the string [n=3]. A string
      value is inserted as it is and any other as JSON with object keys sorted.
      A backslash before [$(] or [${] keeps it literal, a double backslash is
      one backslash, and any other backslash stays;
    - else [s] itself, as a string.

    A reference is a root, one of [inputs], [self], [runtime] and [null],
    followed by segments: [.name], a quoted key in brackets ([['k']], or with
    double quotes) or an index ([[0]]). A quoted key without brackets is
    accepted too. A name is ASCII letters, digits and [_], not starting with a
    digit, and a quoted key ends at its first closing quote. Spaces and tabs are
    allowed inside the parentheses and before a segment, not within one.

    A segment looks up a field of an object, a File or Directory included, as it
    holds it: every field it was given, and those [Ty] fills in; item [n] of an
    array or byte [n] of a string; or, as the last segment, the [length] of an
    array. A lookup that finds nothing (a missing field, an index out of range,
    any segment on null or on a value of another kind), an unknown root, and an
    unclosed [$(] or [${] are [Error.Expr].

    A closed [${...}], and a closed [$(...)] that is not a reference, such as
    [$(1 + 1)], are [Error.Unsupported] for [InlineJavascriptRequirement]. *)
