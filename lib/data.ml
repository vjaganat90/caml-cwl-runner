(** ADTs defined once. Public modules [include] their part ([Error], [Doc],
    [Type], [Schema], [Expr]). This file has no I/O and no functions. *)

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

module Doc = struct
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

  type argument = Literal of string | Binding of binding

  type requirement =
    | Resource of { cores_min : float option }
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

  type command_line_tool = {
    cwl_version : string;
    class_ : string;
    base_command : string list;
    arguments : argument list;
    inputs : input list;
    outputs : output list;
    stdout : string option;
    stdin : string option;
    stderr : string option;
    success_codes : int list;
    requirements : requirement list;
    hints : requirement list;
  }
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
