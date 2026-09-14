open Harness

let example_cases =
  [
    example_case ~docker:true "cl_basic_generation" "bwa-mem-tool.cwl"
      "bwa-mem-job.json"
      [
        "bwa";
        "mem";
        "-t";
        "2";
        "-I";
        "1,2,3,4";
        "-m";
        "3";
        "chr20.fa";
        "example_human_Illumina.pe_1.fastq";
        "example_human_Illumina.pe_2.fastq";
      ];
    example_case ~docker:true "cl_optional_inputs_missing" "cat1-testcli.cwl"
      "cat-job.json" [ "cat"; "hello.txt" ];
    example_case "cl_optional_bindings_provided" "cat1-testcli.cwl"
      "cat-n-job.json"
      [ "cat"; "-n"; "hello.txt" ];
    example_case "nested_prefixes_arrays" "binding-test.cwl" "bwa-mem-job.json"
      [
        "bwa";
        "mem";
        "chr20.fa";
        "-XXX";
        "-YYY";
        "example_human_Illumina.pe_1.fastq";
        "-YYY";
        "example_human_Illumina.pe_2.fastq";
      ];
  ]

open QCheck2

let prop_boolean_flag =
  Test.make ~name:"boolean flag" ~count:100 Gen.bool (fun b ->
      argv_ok
        [ bound "flag" ~ty:Cwl.Type.Boolean ~prefix:"-f" ]
        [ ("flag", Cwl.Type.Vbool b) ]
        (if b then [ "echo"; "-f" ] else [ "echo" ]))

let prop_optional_null_silent =
  Test.make ~name:"optional null is silent" ~count:20 Gen.unit (fun () ->
      argv_ok
        [
          bound "msg"
            ~ty:(Cwl.Type.Union [ Cwl.Type.String; Cwl.Type.Null ])
            ~prefix:"--msg";
        ]
        [ ("msg", Cwl.Type.Vnull) ]
        [ "echo" ])

let prop_item_separator =
  Test.make ~name:"itemSeparator joins" ~count:50
    Gen.(list_size (1 -- 5) (string_size ~gen:printable (1 -- 4)))
    (fun xs ->
      argv_ok
        [
          bound "arr"
            ~ty:
              (Cwl.Type.Array { items = Cwl.Type.String; item_binding = None })
            ~prefix:"-I" ~item_separator:",";
        ]
        [ ("arr", strings xs) ]
        [ "echo"; "-I"; String.concat "," xs ])

let prop_separate_false =
  Test.make ~name:"separate:false concatenates" ~count:50
    Gen.(string_size ~gen:printable (1 -- 8))
    (fun s ->
      argv_ok
        [ bound "v" ~prefix:"-i" ~separate:false ]
        [ ("v", Cwl.Type.Vstring s) ]
        [ "echo"; "-i" ^ s ])

let prop_base_command_prefix =
  Test.make ~name:"argv starts with baseCommand" ~count:30
    Gen.(list_size (1 -- 3) (string_size ~gen:printable (1 -- 6)))
    (fun base -> argv_ok ~base_command:base [] [] base)

let prop_position_order =
  Test.make ~name:"numeric position order" ~count:50
    Gen.(pair (int_range (-5) 5) (int_range (-5) 5))
    (fun (p1, p2) ->
      match
        argv
          [ bound "a" ~position:p1; bound "b" ~position:p2 ]
          [ ("a", Cwl.Type.Vstring "A"); ("b", Cwl.Type.Vstring "B") ]
      with
      | Error _ -> false
      | Ok argv ->
          let rest = match argv with "echo" :: r -> r | r -> r in
          if p1 < p2 then rest = [ "A"; "B" ]
          else if p2 < p1 then rest = [ "B"; "A" ]
          else rest = [ "A"; "B" ] || rest = [ "B"; "A" ])

let prop_param_ref_cores =
  Test.make ~name:"$(runtime.cores) roundtrips" ~count:40
    Gen.(int_range 1 16)
    (fun n ->
      let ctx =
        {
          Cwl.Expr.inputs = [];
          self = Cwl.Type.Vnull;
          runtime = Cwl.Expr.runtime_with_cores (float_of_int n);
        }
      in
      match Cwl.Expr.Param_ref.eval ~ctx ~expr:"$(runtime.cores)" with
      | Ok (Cwl.Type.Vint k) -> Int64.to_int k = n
      | Ok (Cwl.Type.Vfloat f) -> int_of_float f = n
      | _ -> false)

let prop_integral_yaml =
  Test.make ~name:"integral YAML is Int" ~count:100
    Gen.(int_range (-10_000) 10_000)
    (fun n ->
      match Cwl.Doc.load_string (string_of_int n) with
      | Ok (Cwl.Doc.Int m) -> Int64.to_int m = n
      | _ -> false)

let prop_docker_diagnosed =
  Test.make ~name:"unimplemented keys are diagnosed" ~count:1 Gen.unit
    (fun () ->
      let src =
        {|
cwlVersion: v1.2
class: CommandLineTool
hints:
  DockerRequirement:
    dockerPull: alpine
baseCommand: echo
inputs: []
outputs: []
stdout: out.txt
|}
      in
      match Cwl.Doc.load_string src with
      | Error _ -> false
      | Ok doc -> (
          match Cwl.Schema.command_line_tool doc with
          | Error _ -> false
          | Ok ann ->
              has_feature "DockerRequirement" ann.diagnostics
              && List.for_all
                   (fun f -> not (has_feature f ann.diagnostics))
                   [ "stdout"; "outputs" ]))

let prop_tests =
  [
    prop_boolean_flag;
    prop_optional_null_silent;
    prop_item_separator;
    prop_separate_false;
    prop_base_command_prefix;
    prop_position_order;
    prop_param_ref_cores;
    prop_integral_yaml;
    prop_docker_diagnosed;
  ]

let glob_root = "/out"

let glob_ok files pattern expected () =
  let fs = mem_fs ~root:glob_root files in
  match Cwl.Glob.glob fs ~root:glob_root ~pattern with
  | Error e -> Alcotest.fail (Cwl.Error.to_string e)
  | Ok got -> Alcotest.(check (list string)) pattern expected got

let glob_err files pattern () =
  let fs = mem_fs ~root:glob_root files in
  match Cwl.Glob.glob fs ~root:glob_root ~pattern with
  | Error (Cwl.Error.Runtime _) -> ()
  | Error e ->
      Alcotest.fail ("expected Runtime error, got " ^ Cwl.Error.to_string e)
  | Ok hits -> Alcotest.fail ("expected error, got " ^ String.concat "," hits)

let run_tool tool job =
  Eio_main.run @@ fun env ->
  Cwl.run (Cwl.Runtime.local env) ~tool_path:(fixture tool)
    ~job_path:(fixture job) ()

let file_basename v =
  match v with Cwl.Type.Vfile f -> f.Cwl.Type.basename | _ -> None

let file_bytes v =
  match v with
  | Cwl.Type.Vfile { path = Some p; _ } ->
      In_channel.with_open_text p In_channel.input_all
  | _ -> Alcotest.fail "expected File with path"

let exec_ok name tool job check =
  ( name,
    `Quick,
    fun () ->
      match run_tool tool job with
      | Error e -> Alcotest.fail (Cwl.Error.to_string e)
      | Ok ann -> check ann )

let exec_runtime name tool job =
  ( name,
    `Quick,
    fun () ->
      match run_tool tool job with
      | Error (Cwl.Error.Runtime _) -> ()
      | Error e ->
          Alcotest.fail ("expected Runtime error, got " ^ Cwl.Error.to_string e)
      | Ok _ -> Alcotest.fail "expected Runtime error" )

let exec_unsupported name tool job feature =
  ( name,
    `Quick,
    fun () ->
      match run_tool tool job with
      | Error (Cwl.Error.Unsupported { feature = f }) ->
          Alcotest.(check string) "feature" feature f
      | Error e ->
          Alcotest.fail ("expected Unsupported, got " ^ Cwl.Error.to_string e)
      | Ok _ -> Alcotest.fail "expected Unsupported" )

let exec_cases =
  [
    exec_ok "echo_stdout" "echo-stdout.cwl" "echo-job.json" (fun ann ->
        match Cwl.Type.lookup "example_out" ann.value with
        | Some v ->
            Alcotest.(check (option string))
              "basename" (Some "out.txt") (file_basename v);
            Alcotest.(check string) "bytes" "hello\n" (file_bytes v)
        | None -> Alcotest.fail "missing example_out");
    exec_ok "touch_glob" "touch-glob.cwl" "empty-job.json" (fun ann ->
        match Cwl.Type.lookup "out" ann.value with
        | Some v ->
            Alcotest.(check (option string))
              "basename" (Some "hello.txt") (file_basename v)
        | None -> Alcotest.fail "missing out");
    exec_ok "glob_star" "glob-star.cwl" "empty-job.json" (fun ann ->
        match Cwl.Type.lookup "files" ann.value with
        | Some (Cwl.Type.Varray xs) ->
            let names =
              List.filter_map file_basename xs |> List.sort String.compare
            in
            Alcotest.(check (list string)) "names" [ "a.txt"; "b.txt" ] names
        | _ -> Alcotest.fail "expected File array");
    exec_ok "empty_outputs" "empty-outputs.cwl" "empty-job.json" (fun ann ->
        Alcotest.(check int) "empty" 0 (List.length ann.value));
    exec_ok "success_codes" "success-codes.cwl" "empty-job.json" (fun _ -> ());
    exec_ok "json_output" "json-output.cwl" "empty-job.json" (fun ann ->
        match Cwl.Type.lookup "n" ann.value with
        | Some (Cwl.Type.Vint n) -> Alcotest.(check int64) "n" 1L n
        | _ -> Alcotest.fail "expected n = 1");
    exec_unsupported "docker_requirement_fatal" "docker-req.cwl"
      "empty-job.json" "DockerRequirement";
    exec_runtime "basename_escape" "file-in.cwl" "escape-job.json";
    exec_unsupported "output_eval_fatal" "output-eval.cwl" "empty-job.json"
      "outputEval";
    exec_ok "docker_hint_ok" "docker-hint.cwl" "echo-job.json" (fun ann ->
        Alcotest.(check bool)
          "DockerRequirement diagnosed" true
          (has_feature "DockerRequirement" ann.diagnostics);
        match Cwl.Type.lookup "example_out" ann.value with
        | Some v -> Alcotest.(check string) "bytes" "hello\n" (file_bytes v)
        | None -> Alcotest.fail "missing example_out");
  ]

let with_runtime f = Eio_main.run @@ fun env -> f (Cwl.Runtime.local env)

let expect_ok = function
  | Ok v -> v
  | Error e -> Alcotest.fail (Cwl.Error.to_string e)

let expect_runtime = function
  | Error (Cwl.Error.Runtime _) -> ()
  | Error e ->
      Alcotest.fail ("expected Runtime error, got " ^ Cwl.Error.to_string e)
  | Ok () -> Alcotest.fail "expected Runtime error"

let runtime_setup (module R : Cwl.Runtime.RUNTIME) =
  let parent = expect_ok (R.mkdtemp ~prefix:"ccr-conf-") in
  let parent = expect_ok (R.abspath parent) in
  let root = Filename.concat parent "out" in
  expect_ok (R.mkdir_p root);
  (parent, root)

let confined_inside () =
  with_runtime @@ fun (module R : Cwl.Runtime.RUNTIME) ->
  let _parent, root = runtime_setup (module R) in
  let file = Filename.concat root "a.txt" in
  expect_ok (R.write_file file "x");
  expect_ok (R.confined ~roots:[ root ] ~path:file)

let confined_rejects_sibling () =
  with_runtime @@ fun (module R : Cwl.Runtime.RUNTIME) ->
  let parent, root = runtime_setup (module R) in
  let evil = Filename.concat parent "out-evil" in
  expect_ok (R.mkdir_p evil);
  let file = Filename.concat evil "a.txt" in
  expect_ok (R.write_file file "x");
  expect_runtime (R.confined ~roots:[ root ] ~path:file)

let confined_rejects_dotdot () =
  with_runtime @@ fun (module R : Cwl.Runtime.RUNTIME) ->
  let parent, root = runtime_setup (module R) in
  let other = Filename.concat parent "other" in
  expect_ok (R.mkdir_p other);
  let file = Filename.concat other "a.txt" in
  expect_ok (R.write_file file "x");
  let via_dotdot = Filename.concat root (Filename.concat ".." "other/a.txt") in
  expect_runtime (R.confined ~roots:[ root ] ~path:via_dotdot)

let runtime_cases =
  [
    ("confined_inside", `Quick, confined_inside);
    ("confined_rejects_sibling", `Quick, confined_rejects_sibling);
    ("confined_rejects_dotdot", `Quick, confined_rejects_dotdot);
  ]

let glob_cases =
  [
    ("literal", `Quick, glob_ok [ "a.txt" ] "a.txt" [ "/out/a.txt" ]);
    ( "star",
      `Quick,
      glob_ok
        [ "a.txt"; "b.txt"; "c.dat" ]
        "*.txt"
        [ "/out/a.txt"; "/out/b.txt" ] );
    ("question", `Quick, glob_ok [ "a.txt"; "ab.txt" ] "?.txt" [ "/out/a.txt" ]);
    ( "class",
      `Quick,
      glob_ok
        [ "a.txt"; "b.txt"; "c.txt" ]
        "[ab].txt"
        [ "/out/a.txt"; "/out/b.txt" ] );
    ( "subdir",
      `Quick,
      glob_ok
        [ "dir/x.txt"; "dir/y.txt" ]
        "dir/*"
        [ "/out/dir/x.txt"; "/out/dir/y.txt" ] );
    ("hidden not matched", `Quick, glob_ok [ ".foo"; "bar" ] "*" [ "/out/bar" ]);
    ("missing is empty", `Quick, glob_ok [ "a.txt" ] "nope" []);
    ("absolute rejected", `Quick, glob_err [ "a.txt" ] "/etc/passwd");
    ("dotdot rejected", `Quick, glob_err [ "a.txt" ] "../x");
    ("outdir itself", `Quick, glob_ok [ "a.txt" ] "." [ "/out" ]);
  ]

let () =
  Alcotest.run "cwl"
    [
      ("bind_examples", example_cases);
      ( "bind_properties",
        List.map (QCheck_alcotest.to_alcotest ~speed_level:`Quick) prop_tests );
      ("glob", glob_cases);
      ("runtime", runtime_cases);
      ("execute", exec_cases);
    ]
