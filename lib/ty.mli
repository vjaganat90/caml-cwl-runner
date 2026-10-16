(** CWL types ([t]), the runtime values ([value]) they describe, and the
    operations the runner needs on both: checking a value against a type,
    reading a document into values, walking nested values, and printing them
    (for a command line, or as JSON). Inside the library this module is [Ty]; it
    is exported as [Cwl.Type], and its types are declared in [Data.Type].

    The types are the Schema Salad primitives, [File], [Directory], arrays and
    unions; an optional type is a union with [Null]. There is no record, enum or
    [Any] type: [Schema] reads each as [String], with a diagnostic. [Int] and
    [Long] share [Vint] ([int64]), [Float] and [Double] share [Vfloat], and no
    range or precision is checked. An array type carries the [inputBinding]
    written in it as [item_binding]. The type [binding] is defined here, not in
    [Schema] (which parses types and bindings), so this module needs nothing
    from [Schema]. A [Vrecord] is an object that is not a File or Directory,
    such as a nested object in a job (the input object); no type describes it.
    An [object_] is a job or an output object: an association list from
    parameter id to value, in order.

    A {i leaf} is a value that is not an array or a record. File and Directory
    are leaves, and hold only the fields of [file] and [directory]: reading a
    document into values drops any other field ([dirname], [format], [contents],
    [secondaryFiles], and for a Directory [basename] and [listing]). The walkers
    [map], [map_result], [fold] and [fold_map] visit the leaves in
    {i walk order}: depth first, in list order.

    Pure: no I/O (a document arrives already loaded, as an
    [Untyped_tree.value]), no expression evaluation, no staging or hashing (that
    is [Workdir]). A failure is an [Error.t]. Spec: CWL v1.2 Process (File,
    Directory, [InputParameter.default], [inputs]); CommandLineTool
    (CommandLineBinding, which says how a value is written on the command line);
    Parameter references (String interpolation). *)

include module type of Data.Type

val default_binding : binding
(** The binding with every field at its CWL default: [position] 0, [separate]
    true, and no [prefix], [item_separator] or [value_from]. [Schema] returns it
    for an [inputBinding] that is [null], and takes its [position] for a binding
    that omits one; [Bind] uses it for array items that have no binding of their
    own, and [Command_line_tool] for a string in [arguments], with that string
    as its [value_from]. *)

val is_optional : t -> bool
(** [is_optional t] is [true] when [t] admits [null]: it is [Null], or a [Union]
    with an optional member at any depth. An array is never optional, whatever
    its items. *)

val matches : t -> value -> bool
(** [matches t v] is [true] when [v] is a valid value of type [t]. [Int] and
    [Long] take an integer, [Float] and [Double] any number, [String] only a
    string; an array needs every item to match, a [Union] one member, and no
    type takes a [Vrecord]. [apply_defaults_and_check] uses it on job inputs and
    [Cwl.run] on the entries of a [cwl.output.json]; [Bind] uses it to pick the
    member of a union that fits a value. *)

val type_name : t -> string
(** [type_name t] is the name of [t] in an error message: [int], [File],
    [string[]] for an array of strings, [[null, string]] for a union. An array's
    binding is not shown. *)

val value_kind : value -> string
(** [value_kind v] names what kind of value [v] is, for an error message: the
    CWL name of a scalar, [File] or [Directory], and [array] or [record] with no
    item type. A [Vint] is [int] and a [Vfloat] is [float], never [long] or
    [double]. *)

val file_loc : file -> string option
(** [file_loc f] is the [location] of [f], else its [path], else [None]. It is
    where to read [f] from when staging it. *)

val dir_loc : directory -> string option
(** Like [file_loc], for a Directory. *)

val file_path : file -> string option
(** [file_path f] is the [path] of [f], else its [location] as written
    ([file://] included), else [None]: the opposite preference to [file_loc]. It
    is the path a command line shows, and the one [Workdir] relocates, confines
    and hashes. *)

val dir_path : directory -> string option
(** Like [file_path], for a Directory. *)

val map : (value -> value) -> value -> value
(** [map f v] is [v] with [f] applied to each leaf. Arrays and records keep
    their shape and keys. [f] never sees an array or a record, and what it
    returns is not walked again. *)

val map_result :
  (value -> (value, Error.t) result) -> value -> (value, Error.t) result
(** Like [map] for an [f] that can fail. The first [Error] in walk order is the
    result, and [f] is not called on the leaves after it. *)

val fold : ('a -> value -> 'a) -> 'a -> value -> 'a
(** [fold f acc v] threads [acc] through [f] at each leaf of [v] in walk order,
    as [List.fold_left] does over a list. An empty array or record has no
    leaves, so [f] never sees it. *)

val fold_map :
  ('a -> value -> (value * 'a, Error.t) result) ->
  'a ->
  value ->
  (value * 'a, Error.t) result
(** [fold_map f acc v] is [map_result] with state: [f] is called on each leaf in
    walk order with the accumulator so far, and returns the new leaf and the
    next accumulator. The result is the rebuilt [v] and the last accumulator;
    the first [Error] stops the walk. [Workdir] threads the basenames it has
    already staged through it, to catch two Files staged under one name. *)

val fill_file_paths : value -> value
(** [fill_file_paths v] sets the derived fields that a File or Directory in [v],
    at any depth, leaves unset, and keeps those already set. The spec has an
    implementation fill in [path], [basename], [nameroot] and [nameext]. A
    missing [path] is the last component of [location] read as a path by
    [Location.to_path] (a name, not a full path, with percent escapes decoded),
    a File's missing [basename] is the last component of [path], and its missing
    [nameroot] and [nameext] split [basename] at the last dot. So a [location]
    of [file:///d/a.tar.gz] gives [path] and [basename] [a.tar.gz], [nameroot]
    [a.tar] and [nameext] [.gz]; [.cshrc] has an empty [nameext]. [checksum] and
    [size] are not computed here. *)

val value_of_tree : string -> t -> Untyped_tree.value -> (value, Error.t) result
(** [value_of_tree param ty doc] reads [doc] as a value of type [ty]; [Schema]
    uses it for an input's [default], and [param] is the input id that errors
    name. A union takes its first member that reads: [Null] members are tried
    last, or first when [doc] is [null]. Numbers read loosely: an integer a
    [Float] or [Double], any number a [String]. A File or Directory reads from
    an object whose optional [class] must match, drops the fields it does not
    model, and gets [fill_file_paths].

    A mismatch is [Error.Type], naming in [expected] the type that failed (the
    item type for a bad array item, the whole union for a union) and printing in
    [got] the part of [doc] it failed on (just the [class], when that is what is
    wrong). A File or Directory field of the wrong type is [Error.Schema] at
    [param.field], except inside a union, which reports [Error.Type] for the
    whole union instead. *)

val object_of_tree : Untyped_tree.value -> (object_, Error.t) result
(** [object_of_tree doc] reads a job or a [cwl.output.json] without knowing any
    types: [null] is the empty object, and each key of [doc] is an entry, in
    order. A nested object with [class: File] or [class: Directory] reads as a
    File or Directory (through [fill_file_paths]), or stays a plain record if
    one of its fields has the wrong type. Types are checked later, by
    [apply_defaults_and_check] for a job. A [doc] that is neither an object nor
    [null] is [Error.Schema] at [/]. *)

val apply_defaults_and_check :
  input_spec list -> object_ -> (object_, Error.t) result
(** [apply_defaults_and_check specs job] is [job] completed and checked against
    [specs], the [input_spec]s of a tool (from [Command_line_tool.input_specs]):
    one entry per [input_spec], in the order of [specs], so a key of [job] that
    no [input_spec] declares is dropped; every File and Directory goes through
    [fill_file_paths]. A missing or [null] entry takes the [default] of its
    [input_spec], else [null] if its type is optional, as the CWL spec requires
    of a missing input. It fails at the first [input_spec], in order, that has
    no value ([Error.Missing]) or a value that does not match ([Error.Type],
    with [expected] the [type_name] of its [ty] and [got] the [value_kind] of
    the value). *)

val string_of_value : value -> string
(** [string_of_value v] is the text [v] shows on a command line; [Bind] builds
    the words of argv from it. A string is itself, even inside an array or a
    record, and [null] and booleans print as in JSON. A number is a plain
    decimal, never an exponent: a float prints with 10 fractional digits, minus
    trailing zeros but keeping at least one fractional digit, except that an
    integral float below 1e15 in magnitude has no fraction ([3.] is [3], [1e-12]
    is [0.0]). An array prints as [[a, b]] and a record as [{k: v}]. A File or
    Directory is its [file_path] or [dir_path], or the empty string if unset. *)

val to_json : value -> string
(** [to_json v] is [v] as compact JSON, with record keys in their stored order.
    A File or Directory is an object of [class] and then the fields that are
    set, in the order of [file] or [directory], with [file://] put before its
    [location] unless that starts with [file:]. A float prints with no fraction
    when it is integral with magnitude below 1e15, else through
    [string_of_float], which keeps 12 significant digits and prints a non-finite
    float as [nan], [inf] or [-inf] (not valid JSON). *)

val to_json_sorted : value -> string
(** Like [to_json], with the keys of every record, File and Directory sorted
    bytewise. [Expr] uses it for the text a non-string value takes inside an
    interpolated string, where the spec sorts object entries by key. A string,
    even a top-level one, stays quoted: [Expr] writes it bare itself. *)

val object_to_json : object_ -> string
(** [object_to_json o] is [to_json] of [o] as a record, keys in the order of
    [o]: the output object [ccr] prints on stdout. *)
