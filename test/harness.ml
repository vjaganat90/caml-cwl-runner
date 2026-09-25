(** Shared construction for the Alcotest suites: tool records, fixture files,
    and File lookups. Does not define cases and does not spawn. *)

let mk_tool ?(base_command = [ "echo" ]) ?(arguments = []) inputs =
  {
    Cwl.Command_line_tool.cwl_version = "v1.2";
    class_ = "CommandLineTool";
    base_command;
    arguments;
    inputs;
    outputs = [];
    stdout = None;
    stdin = None;
    stderr = None;
    success_codes = [ 0 ];
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
    (mk_tool ~base_command inputs)
    job Cwl.Expr.default_runtime

let argv_ok ?base_command inputs job expected =
  match argv ?base_command inputs job with
  | Ok got -> got = expected
  | Error _ -> false

let has_feature feature diags =
  List.exists (fun d -> d.Cwl.Error.feature = feature) diags

let fixture name = Filename.concat "fixtures" name

let read_fixture name =
  In_channel.with_open_text (fixture name) In_channel.input_all

let write_text path text =
  Out_channel.with_open_text path (fun oc -> output_string oc text);
  path

let copy_fixture dir name dest =
  write_text (Filename.concat dir dest) (read_fixture name)

let substitute text needle repl =
  let nlen = String.length needle in
  if nlen = 0 then text
  else
    let buf = Buffer.create (String.length text) in
    let rec go i =
      let rest = String.length text - i in
      if rest < nlen then Buffer.add_substring buf text i rest
      else if String.sub text i nlen = needle then (
        Buffer.add_string buf repl;
        go (i + nlen))
      else (
        Buffer.add_char buf text.[i];
        go (i + 1))
    in
    go 0;
    Buffer.contents buf

let with_runtime f = Eio_main.run @@ fun env -> f (Cwl.Runtime.local env)

let docker_ready () =
  let bin = Filename.quote (Cwl.Runtime.docker_executable ()) in
  match Unix.system (bin ^ " info >/dev/null 2>&1") with
  | Unix.WEXITED 0 -> true
  | _ -> false

let require_docker () =
  if not (docker_ready ()) then (
    Printf.eprintf "docker is not available\n";
    Alcotest.skip ())

let expect_ok = function
  | Ok v -> v
  | Error e -> Alcotest.fail (Cwl.Error.to_string e)

let expect_runtime = function
  | Error (Cwl.Error.Runtime _) -> ()
  | Error e ->
      Alcotest.fail ("expected Runtime error, got " ^ Cwl.Error.to_string e)
  | Ok _ -> Alcotest.fail "expected Runtime error"

let file_basename = function
  | Cwl.Type.Vfile f -> f.Cwl.Type.basename
  | _ -> None

let dir_path = function Cwl.Type.Vdir d -> Cwl.Type.dir_path d | _ -> None

let mem_sub ~sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let file_bytes = function
  | Cwl.Type.Vfile { path = Some p; _ } ->
      In_channel.with_open_text p In_channel.input_all
  | _ -> Alcotest.fail "expected File with path"

let lookup_file id ann =
  match Cwl.Type.lookup id ann.Cwl.Error.value with
  | Some (Cwl.Type.Vfile f) -> f
  | _ -> Alcotest.fail ("expected File " ^ id)

let lookup_file_basename id expected ann =
  Alcotest.(check (option string))
    "basename" (Some expected) (lookup_file id ann).Cwl.Type.basename

let lookup_file_path id ann = (lookup_file id ann).Cwl.Type.path

let lookup_bytes id expected ann =
  match Cwl.Type.lookup id ann.Cwl.Error.value with
  | Some v -> Alcotest.(check string) "bytes" expected (file_bytes v)
  | None -> Alcotest.fail ("missing " ^ id)

let lookup_int id expected ann =
  match Cwl.Type.lookup id ann.Cwl.Error.value with
  | Some (Cwl.Type.Vint n) -> Alcotest.(check int64) id expected n
  | _ -> Alcotest.fail ("expected " ^ id ^ " int")

let lookup_null id ann =
  match Cwl.Type.lookup id ann.Cwl.Error.value with
  | Some Cwl.Type.Vnull -> ()
  | _ -> Alcotest.fail "expected null"

let lookup_empty_array id ann =
  match Cwl.Type.lookup id ann.Cwl.Error.value with
  | Some (Cwl.Type.Varray []) -> ()
  | _ -> Alcotest.fail "expected empty array"

let lookup_basenames id expected ann =
  match Cwl.Type.lookup id ann.Cwl.Error.value with
  | Some (Cwl.Type.Varray xs) ->
      let names =
        List.filter_map file_basename xs |> List.sort String.compare
      in
      Alcotest.(check (list string)) "names" expected names
  | _ -> Alcotest.fail "expected File array"

let lookup_dir id ann =
  match Cwl.Type.lookup id ann.Cwl.Error.value with
  | Some v -> (
      match dir_path v with
      | Some p ->
          Alcotest.(check string) "basename" id (Filename.basename p);
          Alcotest.(check bool) "is dir" true (Sys.is_directory p)
      | None -> Alcotest.fail "Directory missing path")
  | _ -> Alcotest.fail "expected Directory"

let command_line tool job =
  match Cwl.command_line (fixture tool) (fixture job) with
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

let mem_fs ~root files : (module Cwl.Glob.FS) =
  let trim p =
    let n = String.length p in
    if n > 1 && p.[n - 1] = '/' then String.sub p 0 (n - 1) else p
  in
  let root = trim root in
  let files =
    List.map
      (fun f ->
        if f = root || String.starts_with ~prefix:(root ^ "/") f then f
        else root ^ "/" ^ f)
      files
  in
  let rec parents p acc =
    match String.rindex_opt p '/' with
    | None -> acc
    | Some 0 -> "/" :: acc
    | Some i ->
        let d = String.sub p 0 i in
        parents d (d :: acc)
  in
  let dirs =
    root :: List.concat_map (fun f -> parents f []) files
    |> List.sort_uniq String.compare
  in
  (module struct
    let exists p = List.mem p files || List.mem p dirs
    let is_dir p = List.mem p dirs
    let realpath p = Ok (trim p)

    let read_dir p =
      let prefix = if p = "/" then "/" else p ^ "/" in
      let plen = String.length prefix in
      let names =
        List.filter_map
          (fun q ->
            if String.starts_with ~prefix q then
              let rest = String.sub q plen (String.length q - plen) in
              if rest = "" || String.contains rest '/' then None else Some rest
            else None)
          (files @ dirs)
        |> List.sort_uniq String.compare
      in
      Ok names
  end)
