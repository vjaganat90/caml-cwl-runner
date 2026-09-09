(** Shared construction for argv properties. Tests name the binding and the
    expected tokens; this file owns the CommandLineTool record. *)

let mk_tool ?(base_command = [ "echo" ]) ?(arguments = []) inputs =
  {
    Cwl.Schema.cwl_version = "v1.2";
    class_ = "CommandLineTool";
    base_command;
    arguments;
    inputs;
    outputs = [];
    stdout = None;
    stdin = None;
    stderr = None;
    requirements = [];
    hints = [];
  }

let binding ?(position = 0) ?prefix ?(separate = true) ?item_separator
    ?value_from () =
  {
    Cwl.Type.position = Cwl.Type.Pos position;
    prefix;
    separate;
    item_separator;
    value_from;
  }

let bound ?(ty = Cwl.Type.String) ?prefix ?separate ?item_separator
    ?(position = 0) id =
  {
    Cwl.Schema.id;
    ty;
    default = None;
    input_binding =
      Some (binding ~position ?prefix ?separate ?item_separator ());
    unimplemented = [];
  }

let strings xs = Cwl.Type.Varray (List.map (fun s -> Cwl.Type.Vstring s) xs)

let argv ?(base_command = [ "echo" ]) inputs job =
  Cwl.Bind.argv
    (module Cwl.Expr.Param_ref)
    ~tool:(mk_tool ~base_command inputs)
    ~inputs:job ~runtime:Cwl.Expr.default_runtime

let argv_ok ?base_command inputs job expected =
  match argv ?base_command inputs job with
  | Ok got -> got = expected
  | Error _ -> false

let has_feature feature diags =
  List.exists (fun d -> d.Cwl.Error.feature = feature) diags

let fixture name = Filename.concat "fixtures" name

let command_line tool job =
  match Cwl.command_line ~tool_path:(fixture tool) ~job_path:(fixture job) with
  | Error e -> Alcotest.fail (Cwl.Error.to_string e)
  | Ok ann -> ann

let conformance_args = function
  | "python" :: script :: rest when Filename.basename script = "args.py" -> rest
  | xs -> xs

let example ?(docker = false) name tool job expected () =
  let ann = command_line tool job in
  Alcotest.(check (list string)) name expected (conformance_args ann.value);
  if docker then
    Alcotest.(check bool)
      "DockerRequirement diagnosed" true
      (has_feature "DockerRequirement" ann.diagnostics)

let example_case ?docker name tool job expected =
  (name, `Quick, example ?docker name tool job expected)
