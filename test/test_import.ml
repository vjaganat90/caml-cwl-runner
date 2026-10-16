(** Document loading: integral YAML, [$import], [$include], [$graph], and field
    types. Reads fixture files into an in-memory [FILE], or parses inline YAML.
    Does not spawn. *)

open Harness
open QCheck2

let prop_integral_yaml =
  Test.make ~name:"integral YAML is Int" ~count:100
    Gen.(int_range (-10_000) 10_000)
    (fun n ->
      match Cwl.Untyped_tree.of_yaml_string (string_of_int n) with
      | Ok (Cwl.Untyped_tree.Int m) -> Int64.to_int m = n
      | _ -> false)

let mem_files files =
  let module F = struct
    let read_file path =
      match List.assoc_opt path files with
      | Some text -> Ok text
      | None ->
          Error (Cwl.Error.Parse { path = Some path; message = "not found" })
  end in
  (module F : Cwl.Untyped_tree.FILE)

let doc name = read_fixture ("import/" ^ name)

let import_table () =
  let load files path ?fragment () =
    let (module F) = mem_files files in
    match Cwl.Untyped_tree.load (module F) path with
    | Error _ as e -> e
    | Ok tree -> Cwl.Document.of_tree ?fragment tree
  in
  let command = function
    | Cwl.Document.Command_line_tool tool -> tool.base_command
    | Cwl.Document.Workflow _ -> []
  in
  let show_doc = function Ok _ -> "ok" | Error e -> Cwl.Error.to_string e in
  let expect_cmd name got argv =
    match got with
    | Ok ann ->
        Alcotest.(check (list string)) name argv (command ann.Cwl.Error.value)
    | Error e -> Alcotest.failf "%s: %s" name (Cwl.Error.to_string e)
  in
  let echo = doc "echo.cwl" in
  expect_cmd "include"
    (load
       [
         ("/w/tool.cwl", doc "include-tool.cwl"); ("/w/cmd.txt", doc "cmd.txt");
       ]
       "/w/tool.cwl" ())
    [ "echo" ];
  expect_cmd "import"
    (load
       [ ("/w/root.cwl", doc "root.cwl"); ("/w/echo.cwl", echo) ]
       "/w/root.cwl" ())
    [ "echo" ];
  expect_cmd "$base"
    (load
       [ ("/w/here.cwl", doc "here.cwl"); ("/w/other/echo.cwl", echo) ]
       "/w/here.cwl" ())
    [ "echo" ];
  (match
     load
       [ ("/w/a.cwl", doc "cycle-a.cwl"); ("/w/b.cwl", doc "cycle-b.cwl") ]
       "/w/a.cwl" ()
   with
  | Error (Cwl.Error.Schema { message; _ })
    when String.starts_with ~prefix:"import cycle" message ->
      ()
  | other -> Alcotest.failf "cycle: %s" (show_doc other));
  (match load [ ("/w/m.cwl", doc "missing.cwl") ] "/w/m.cwl" () with
  | Error (Cwl.Error.Parse _) -> ()
  | other -> Alcotest.failf "missing: %s" (show_doc other));
  expect_cmd "#main"
    (load [ ("/w/packed.cwl", doc "packed-main.cwl") ] "/w/packed.cwl" ())
    [ "echo" ];
  expect_cmd "fragment"
    (load
       [ ("/w/packed.cwl", doc "packed-fragment.cwl") ]
       "/w/packed.cwl" ~fragment:"other" ())
    [ "true" ];
  (match
     load [ ("/w/packed.cwl", doc "packed-none.cwl") ] "/w/packed.cwl" ()
   with
  | Error (Cwl.Error.Schema { message; _ })
    when String.starts_with ~prefix:"no entry" message ->
      ()
  | other -> Alcotest.failf "no entry: %s" (show_doc other));
  (match load [ ("/w/h.cwl", doc "remote.cwl") ] "/w/h.cwl" () with
  | Error (Cwl.Error.Unsupported { feature = "remote $import" }) -> ()
  | other -> Alcotest.failf "remote: %s" (show_doc other));
  (match load [ ("/w/bad.cwl", doc "bad-version.cwl") ] "/w/bad.cwl" () with
  | Error (Cwl.Error.Schema { path = "cwlVersion"; _ }) -> ()
  | other -> Alcotest.failf "version: %s" (show_doc other));
  (match load [ ("/w/ns.cwl", doc "namespaces.cwl") ] "/w/ns.cwl" () with
  | Ok ann ->
      Alcotest.(check bool)
        "namespaces" false
        (has_feature "$namespaces" ann.diagnostics)
  | Error e -> Alcotest.fail (Cwl.Error.to_string e));
  match load [ ("/w/wf.cwl", doc "workflow.cwl") ] "/w/wf.cwl" () with
  | Ok { value = Cwl.Document.Workflow _; diagnostics } ->
      Alcotest.(check bool) "workflow" true (has_feature "Workflow" diagnostics)
  | other -> Alcotest.failf "workflow: %s" (show_doc other)

let tool body =
  "cwlVersion: v1.2\nclass: CommandLineTool\nbaseCommand: echo\noutputs: []\n"
  ^ body

(* A field of the wrong type is a schema error at its path: one row per reader
   (string, boolean, int) and per document level. A [null] field is absent,
   a list one too. *)
let field_rows =
  [
    ( tool "inputs: {x: {type: string, inputBinding: {prefix: 5}}}",
      "schema error at inputs.x.inputBinding.prefix: expected string, got 5" );
    ( tool "inputs: [{id: x, type: string, inputBinding: {separate: 'no'}}]",
      "schema error at inputs[0].inputBinding.separate: expected boolean, got \
       \"no\"" );
    ( tool
        "inputs: {f: {type: File, default: {class: File, location: a, size: \
         b}}}",
      "schema error at f.size: expected int, got \"b\"" );
    (tool "inputs: {x: {type: string, inputBinding: {prefix: null}}}", "ok");
    (tool "inputs: null\nhints: null", "ok");
    ( "cwlVersion: v1.2\nclass: 5\n",
      "schema error at class: expected string, got 5" );
    ( "cwlVersion: v1.2\n$graph:\n- {id: 5, class: CommandLineTool}\n",
      "schema error at $graph[0].id: expected string, got 5" );
  ]

let field_table () =
  List.iter
    (fun (text, want) ->
      Result.bind (Cwl.Untyped_tree.of_yaml_string text) Cwl.Document.of_tree
      |> Result.fold ~ok:(fun _ -> "ok") ~error:Cwl.Error.to_string
      |> Alcotest.(check string) text want)
    field_rows

(* A plain scalar is resolved by the YAML 1.2 core schema; a quoted one is a
   string. *)
let scalar_table () =
  List.iter
    (fun (text, want) ->
      Cwl.Untyped_tree.of_yaml_string text
      |> Result.fold
           ~ok:(Format.asprintf "%a" Cwl.Untyped_tree.pp)
           ~error:Cwl.Error.to_string
      |> Alcotest.(check string) text want)
    [
      ("no", {|"no"|});
      ("y", {|"y"|});
      ("On", {|"On"|});
      ("yes", {|"yes"|});
      ("True", "true");
      ("FALSE", "false");
      ("~", "null");
      ("", "null");
      ("'true'", {|"true"|});
      ("0x1F", "31");
      ("0o17", "15");
      ("-5", "-5");
      ("0b1", {|"0b1"|});
      ("1_0", {|"1_0"|});
      ("2.0", "2.");
      ("1e3", "1000.");
      ("9007199254740993", "9007199254740993");
      ("[no, 5]", {|["no", 5]|});
      ("a: &x 1\nb: *x", "parse error: aliases are not supported");
    ]

let tests =
  [
    ("imports", [ ("table", `Quick, import_table) ]);
    ("fields", [ ("table", `Quick, field_table) ]);
    ("scalars", [ ("table", `Quick, scalar_table) ]);
    ( "yaml",
      List.map
        (QCheck_alcotest.to_alcotest ~speed_level:`Quick)
        [ prop_integral_yaml ] );
  ]
