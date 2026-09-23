(** ADTs defined once. Public modules [include] their part ([Error],
    [Untyped_tree], [Type], [Schema], [Command_line_tool], [Workflow],
    [Document], [Expr]). No I/O. [Container_outdir.of_string] is the only
    function: a path docker would mount somewhere else is not a [t]. *)

module Error = struct
  type t =
    | Parse of { path : string option; message : string }
    | Schema of { path : string; message : string }
    | Unsupported of { feature : string }
    | Type of { param : string; expected : string; got : string }
    | Expr of { message : string }
    | Missing of { param : string }
    | Runtime of { message : string }

  type diagnostic = {
    feature : string;
    json_path : string;
    in_requirements : bool;
    message : string;
  }

  type 'a annotated = { value : 'a; diagnostics : diagnostic list }
end

module Untyped_tree = struct
  type value =
    | Null
    | Bool of bool
    | Int of int64
    | Float of float
    | String of string
    | Array of value list
    | Object of (string * value) list

  module type FILE = sig
    val read : string -> (string, Error.t) result
  end
end

module Type = struct
  type position = Pos of int | Expr of string

  type binding = {
    position : position;
    prefix : string option;
    separate : bool;
    item_separator : string option;
    value_from : string option;
  }

  type t =
    | Null
    | Boolean
    | Int
    | Long
    | Float
    | Double
    | String
    | File
    | Directory
    | Array of { items : t; item_binding : binding option }
    | Union of t list

  type file = {
    location : string option;
    path : string option;
    basename : string option;
    nameroot : string option;
    nameext : string option;
    checksum : string option;
    size : int64 option;
  }

  type directory = { location : string option; path : string option }

  type value =
    | Vnull
    | Vbool of bool
    | Vint of int64
    | Vfloat of float
    | Vstring of string
    | Vfile of file
    | Vdir of directory
    | Varray of value list
    | Vrecord of (string * value) list

  type object_ = (string * value) list
  type input_spec = { id : string; ty : t; default : value option }
end

module Schema = struct
  type binding = Type.binding
  type position = Type.position
  type cwl_type = Type.t

  type input = {
    id : string;
    ty : cwl_type;
    default : Type.value option;
    input_binding : binding option;
    unimplemented : Error.diagnostic list;
  }

  type docker_image =
    | Pull of string
    | Image_id of string
    | Load of { source : string; name : string option }
    | Import of { source : string; name : string option }
    | Dockerfile of { contents : string; tag : string option }

  module Container_outdir : sig
    type t = private string

    val of_string : string -> (t, string) result
    val to_string : t -> string
  end = struct
    type t = string

    let to_string s = s
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
  }

  type requirement =
    | Resource of { cores_min : float option }
    | Docker of docker
    | Unimplemented of { class_ : string; in_requirements : bool }

  type output_binding = {
    glob : string list;
    unimplemented : Error.diagnostic list;
  }

  type stream = Stdout | Stderr | No_stream

  type output = {
    id : string;
    ty : cwl_type;
    output_binding : output_binding option;
    stream : stream;
    unimplemented : Error.diagnostic list;
  }
end

module Command_line_tool = struct
  type argument = Literal of string | Binding of Schema.binding

  type command_line_tool = {
    cwl_version : string;
    class_ : string;
    base_command : string list;
    arguments : argument list;
    inputs : Schema.input list;
    outputs : Schema.output list;
    stdout : string option;
    stdin : string option;
    stderr : string option;
    success_codes : int list;
    requirements : Schema.requirement list;
    hints : Schema.requirement list;
  }

  type t = command_line_tool
end

module Workflow = struct
  type workflow = {
    cwl_version : string;
    class_ : string;
    inputs : Schema.input list;
    outputs : Schema.output list;
    requirements : Schema.requirement list;
    hints : Schema.requirement list;
  }

  type t = workflow
end

module Document = struct
  type t = Command_line_tool of Command_line_tool.t | Workflow of Workflow.t
end

module Expr = struct
  type runtime = {
    outdir : string;
    tmpdir : string;
    cores : float;
    ram : float;
  }

  type context = { inputs : Type.object_; self : Type.value; runtime : runtime }

  module type ENGINE = sig
    val eval : context -> string -> (Type.value, Error.t) result
  end
end
