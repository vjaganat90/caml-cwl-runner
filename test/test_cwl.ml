(** Alcotest entry: argv examples, QCheck2 properties, in-memory glob, Runtime
    confinement, and [execute_edges] over fixture files. One runner per table;
    fixtures stay on disk. *)

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
      match Cwl.Expr.Param_ref.eval ctx "$(runtime.cores)" with
      | Ok (Cwl.Type.Vint k) -> Int64.to_int k = n
      | Ok (Cwl.Type.Vfloat f) -> int_of_float f = n
      | _ -> false)

let prop_integral_yaml =
  Test.make ~name:"integral YAML is Int" ~count:100
    Gen.(int_range (-10_000) 10_000)
    (fun n ->
      match Cwl.Untyped_tree.load_string (string_of_int n) with
      | Ok (Cwl.Untyped_tree.Int m) -> Int64.to_int m = n
      | _ -> false)

let prop_docker_argv_suffix =
  Test.make ~name:"docker run keeps the CWL argv as a suffix" ~count:40
    Gen.(list_size (int_range 1 6) (string_size ~gen:char (int_range 1 8)))
    (fun raw ->
      let argv = List.map (fun s -> "c" ^ s) raw in
      let spec =
        {
          Cwl.Runtime.bin = "/bin/docker";
          user = "1:1";
          cwd = "/out";
          image = "alpine";
        }
      in
      let got = Cwl.Runtime.docker_run_argv spec argv in
      let n = List.length argv in
      let pre_len = List.length got - n in
      pre_len > 0
      && List.drop pre_len got = argv
      && List.nth got (pre_len - 1) = spec.image)

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
      match Cwl.Untyped_tree.load_string src with
      | Error _ -> false
      | Ok tree -> (
          match Cwl.Command_line_tool.of_tree tree with
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
    prop_docker_argv_suffix;
    prop_docker_diagnosed;
  ]

let glob_root = "/out"

let glob_ok ?roots files pattern expected () =
  let fs = mem_fs ~root:glob_root files in
  match Cwl.Glob.glob fs ?roots glob_root pattern with
  | Error e -> Alcotest.fail (Cwl.Error.to_string e)
  | Ok got -> Alcotest.(check (list string)) pattern expected got

let glob_err files pattern () =
  let fs = mem_fs ~root:glob_root files in
  match Cwl.Glob.glob fs glob_root pattern with
  | Error (Cwl.Error.Runtime _) -> ()
  | Error e ->
      Alcotest.fail ("expected Runtime error, got " ^ Cwl.Error.to_string e)
  | Ok hits -> Alcotest.fail ("expected error, got " ^ String.concat "," hits)

let deep_glob_path n =
  let rec dirs i acc =
    if i = 0 then acc else dirs (i - 1) (("d" ^ string_of_int i) :: acc)
  in
  String.concat "/" (dirs n [] @ [ "leaf.txt" ])

let run_tool tool job =
  Eio_main.run @@ fun env ->
  Cwl.run (Cwl.Runtime.local env) (fixture tool) (fixture job)

let file_attr attr v = match v with Cwl.Type.Vfile f -> attr f | _ -> None
let file_basename = file_attr (fun f -> f.Cwl.Type.basename)

let dir_path v =
  match v with Cwl.Type.Vdir d -> Cwl.Type.dir_path d | _ -> None

let mem_sub ~sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let file_bytes v =
  match v with
  | Cwl.Type.Vfile { path = Some p; _ } ->
      In_channel.with_open_text p In_channel.input_all
  | _ -> Alcotest.fail "expected File with path"

type outcome =
  | Expect_ok of (Cwl.Type.object_ Cwl.Error.annotated -> unit)
  | Expect_ok_or_error of (Cwl.Type.object_ Cwl.Error.annotated -> unit)
  | Expect_runtime
  | Expect_type
  | Expect_schema
  | Expect_unsupported of string

type edge = {
  name : string;
  tool : string;
  job : string;
  outcome : outcome;
  docker : bool;
}

let edge ?(job = "empty-job.json") ?(docker = false) name tool outcome =
  { name; tool; job; outcome; docker }

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

let lookup_file id ann =
  match Cwl.Type.lookup id ann.Cwl.Error.value with
  | Some (Cwl.Type.Vfile f) -> f
  | _ -> Alcotest.fail ("expected File " ^ id)

let lookup_file_basename id expected ann =
  Alcotest.(check (option string))
    "basename" (Some expected) (lookup_file id ann).Cwl.Type.basename

let lookup_file_path id ann = (lookup_file id ann).Cwl.Type.path

let lookup_int id expected ann =
  match Cwl.Type.lookup id ann.Cwl.Error.value with
  | Some (Cwl.Type.Vint n) -> Alcotest.(check int64) id expected n
  | _ -> Alcotest.fail ("expected " ^ id ^ " int")

let lookup_bytes id expected ann =
  match Cwl.Type.lookup id ann.Cwl.Error.value with
  | Some v -> Alcotest.(check string) "bytes" expected (file_bytes v)
  | None -> Alcotest.fail ("missing " ^ id)

let collect_paths acc v =
  Cwl.Type.fold
    (fun acc -> function
      | Cwl.Type.Vfile f -> (
          match Cwl.Type.file_path f with Some p -> p :: acc | None -> acc)
      | Cwl.Type.Vdir d -> (
          match Cwl.Type.dir_path d with Some p -> p :: acc | None -> acc)
      | _ -> acc)
    acc v

let leaks_host p =
  String.starts_with ~prefix:"/etc/" p
  || String.starts_with ~prefix:"/usr/" p
  || p = "/etc" || p = "/usr" || p = "/"

let lookup_dir id ann =
  match Cwl.Type.lookup id ann.Cwl.Error.value with
  | Some v -> (
      match dir_path v with
      | Some p ->
          Alcotest.(check string) "basename" id (Filename.basename p);
          Alcotest.(check bool) "is dir" true (Sys.is_directory p)
      | None -> Alcotest.fail "Directory missing path")
  | _ -> Alcotest.fail "expected Directory"

let execute_edges =
  [
    edge "echo_stdout" "echo-stdout.cwl" ~job:"echo-job.json"
      (Expect_ok
         (fun ann ->
           lookup_file_basename "example_out" "out.txt" ann;
           lookup_bytes "example_out" "hello\n" ann));
    edge "touch_glob" "touch-glob.cwl"
      (Expect_ok (lookup_file_basename "out" "hello.txt"));
    edge "glob_star" "glob-star.cwl"
      (Expect_ok (lookup_basenames "files" [ "a.txt"; "b.txt" ]));
    edge "empty_outputs" "empty-outputs.cwl"
      (Expect_ok
         (fun ann -> Alcotest.(check int) "empty" 0 (List.length ann.value)));
    edge "success_codes" "success-codes.cwl" (Expect_ok (fun _ -> ()));
    edge "json_output" "json-output.cwl" (Expect_ok (lookup_int "n" 1L));
    edge "docker_missing_pull" "docker-no-pull.cwl" Expect_schema;
    edge "docker_pull_not_string" "docker-pull-int.cwl" Expect_schema;
    edge "docker_output_directory" "docker-output-dir.cwl"
      (Expect_unsupported "DockerRequirement.dockerOutputDirectory");
    edge ~docker:true "docker_echo" "docker-req.cwl" (Expect_ok (fun _ -> ()));
    edge ~docker:true "docker_image_id" "docker-image-id.cwl"
      (Expect_ok (fun _ -> ()));
    edge ~docker:true "docker_file" "docker-file.cwl" (Expect_ok (fun _ -> ()));
    edge ~docker:true "docker_json_escape" "docker-json-escape.cwl"
      Expect_runtime;
    edge "output_eval_no_json" "output-eval.cwl"
      (Expect_unsupported "outputEval");
    edge "json_ignores_output_eval" "json-and-eval.cwl"
      (Expect_ok (lookup_int "n" 1L));
    edge "stdin_cat" "stdin-cat.cwl" ~job:"stdin-job.json"
      (Expect_ok (lookup_bytes "out" "hello from file\n"));
    edge "stderr_stream" "stderr-stream.cwl"
      (Expect_ok (lookup_bytes "err" "err\n"));
    edge "dir_glob" "dir-glob.cwl" (Expect_ok (lookup_dir "d"));
    edge "param_ref_glob" "param-ref-glob.cwl" ~job:"param-ref-job.json"
      (Expect_ok (lookup_file_basename "f" "hello.txt"));
    edge "json_relative_file" "json-relative.cwl"
      (Expect_ok
         (fun ann ->
           match lookup_file_path "out" ann with
           | Some p ->
               Alcotest.(check bool)
                 "absolute" true
                 (not (Filename.is_relative p));
               Alcotest.(check string) "basename" "a.txt" (Filename.basename p)
           | None -> Alcotest.fail "File missing path"));
    edge "nameroot_nameext" "nameroot.cwl"
      (Expect_ok
         (fun ann ->
           let f = lookup_file "f" ann in
           Alcotest.(check (option string))
             "nameroot" (Some "foo") f.Cwl.Type.nameroot;
           Alcotest.(check (option string))
             "nameext" (Some ".txt") f.Cwl.Type.nameext;
           let json = Cwl.Type.to_json (Cwl.Type.Vfile f) in
           Alcotest.(check bool)
             "json nameroot" true
             (mem_sub ~sub:"\"nameroot\":\"foo\"" json);
           Alcotest.(check bool)
             "json nameext" true
             (mem_sub ~sub:"\"nameext\":\".txt\"" json)));
    edge "symlink_dir_inside" "symlink-dir.cwl"
      (Expect_ok (lookup_file_basename "f" "a.txt"));
    edge "docker_hint_ok" "docker-hint.cwl" ~job:"echo-job.json"
      (Expect_ok
         (fun ann ->
           Alcotest.(check bool)
             "DockerRequirement diagnosed" true
             (has_feature "DockerRequirement" ann.diagnostics);
           lookup_bytes "example_out" "hello\n" ann));
    edge "glob_required_missing" "glob-missing.cwl" Expect_runtime;
    edge "glob_optional_missing" "glob-optional.cwl"
      (Expect_ok (lookup_null "out"));
    edge "glob_array_empty" "glob-array-empty.cwl"
      (Expect_ok (lookup_empty_array "files"));
    edge "glob_file_too_many" "glob-too-many.cwl" Expect_runtime;
    edge "glob_file_is_dir" "glob-file-is-dir.cwl" Expect_type;
    edge "glob_dir_is_file" "glob-dir-is-file.cwl" Expect_type;
    edge "glob_mixed_star" "glob-mixed-star.cwl" Expect_type;
    edge "glob_list_patterns" "glob-list.cwl"
      (Expect_ok (lookup_basenames "files" [ "a.txt"; "b.dat" ]));
    edge "glob_runtime_outdir" "glob-outdir.cwl"
      (Expect_ok
         (fun ann ->
           match Cwl.Type.lookup "d" ann.value with
           | Some v -> (
               match dir_path v with
               | Some p ->
                   Alcotest.(check bool) "is dir" true (Sys.is_directory p)
               | None -> Alcotest.fail "Directory missing path")
           | _ -> Alcotest.fail "expected Directory"));
    edge "glob_hidden_star" "glob-hidden.cwl"
      (Expect_ok (lookup_basenames "files" [ "vis" ]));
    edge "basename_collision" "two-files.cwl" ~job:"collide-job.json"
      Expect_runtime;
    edge "json_path_over_location" "json-path-wins.cwl"
      (Expect_ok
         (fun ann ->
           match lookup_file_path "f" ann with
           | Some p ->
               Alcotest.(check string) "basename" "a.txt" (Filename.basename p);
               Alcotest.(check bool)
                 "not host path" true
                 (not (String.starts_with ~prefix:"/etc/" p))
           | None -> Alcotest.fail "File missing path"));
    edge "json_extra_int_kept" "json-extra-int.cwl"
      (Expect_ok (lookup_int "n" 1L));
    edge "nameroot_no_dot" "nameroot-plain.cwl"
      (Expect_ok
         (fun ann ->
           let f = lookup_file "f" ann in
           Alcotest.(check (option string))
             "nameroot" (Some "foo") f.Cwl.Type.nameroot;
           Alcotest.(check (option string))
             "nameext" (Some "") f.Cwl.Type.nameext));
    edge "stdout_from_input" "stdout-named.cwl" ~job:"stdout-named-job.json"
      (Expect_ok
         (fun ann ->
           lookup_file_basename "out" "named.txt" ann;
           lookup_bytes "out" "hi\n" ann));
    edge "file_space_name" "file-space.cwl"
      (Expect_ok (lookup_file_basename "f" "a b.txt"));
    edge "json_path_escape" "json-path-escape.cwl" Expect_runtime;
    edge "json_abs_escape" "json-abs-escape.cwl" Expect_runtime;
    edge "json_location_escape" "json-location-escape.cwl" Expect_runtime;
    edge "json_extra_escape" "json-extra-escape.cwl" Expect_runtime;
    edge "basename_escape" "file-in.cwl" ~job:"escape-job.json" Expect_runtime;
    edge "copy_device" "file-in.cwl" ~job:"device-job.json" Expect_runtime;
    edge "copy_directory_as_file" "file-in.cwl" ~job:"dir-as-file-job.json"
      Expect_runtime;
    edge "http_location" "file-in.cwl" ~job:"http-job.json" Expect_runtime;
    edge "glob_symlink_out" "glob-symlink.cwl" Expect_runtime;
    edge "directory_input_argv" "dir-in.cwl" ~job:"dir-in-job.json"
      (Expect_ok
         (fun ann ->
           match Cwl.Type.lookup "out" ann.value with
           | Some v ->
               let p = String.trim (file_bytes v) in
               Alcotest.(check bool) "is dir" true (Sys.is_directory p)
           | None -> Alcotest.fail "missing out"));
    edge "glob_overlap_uniq" "glob-overlap.cwl"
      (Expect_ok (lookup_basenames "files" [ "a.txt" ]));
    edge "json_file_is_dir" "json-file-is-dir.cwl" Expect_type;
    edge "workflow_unsupported" "workflow.cwl" (Expect_unsupported "Workflow");
    edge "glob_starstar_root" "glob-starstar.cwl"
      (Expect_ok_or_error
         (fun ann ->
           let paths =
             List.fold_left (fun acc (_, v) -> collect_paths acc v) [] ann.value
           in
           List.iter
             (fun p ->
               if leaks_host p then Alcotest.fail ("leaked host path " ^ p))
             paths));
  ]

let docker_ready () =
  let bin = Filename.quote (Cwl.Runtime.docker_executable ()) in
  match Unix.system (bin ^ " info >/dev/null 2>&1") with
  | Unix.WEXITED 0 -> true
  | _ -> false

let require_docker () =
  if not (docker_ready ()) then (
    Printf.eprintf "docker is not available\n";
    Alcotest.skip ())

let run_edge (e : edge) () =
  if e.docker then require_docker ();
  let result =
    if not e.docker then run_tool e.tool e.job
    else
      Eio_main.run @@ fun env ->
      let local = Cwl.Runtime.local env in
      let docker image = Cwl.Runtime.docker env image in
      Cwl.run local ~docker (fixture e.tool) (fixture e.job)
  in
  match result with
  | Ok ann -> (
      match e.outcome with
      | Expect_ok f | Expect_ok_or_error f -> f ann
      | Expect_runtime -> Alcotest.fail "expected Runtime error"
      | Expect_type -> Alcotest.fail "expected Type error"
      | Expect_schema -> Alcotest.fail "expected Schema error"
      | Expect_unsupported _ -> Alcotest.fail "expected Unsupported")
  | Error (Cwl.Error.Runtime _ as err) -> (
      match e.outcome with
      | Expect_runtime | Expect_ok_or_error _ -> ()
      | _ ->
          Alcotest.fail
            ("unexpected Runtime error: " ^ Cwl.Error.to_string err))
  | Error (Cwl.Error.Type _) -> (
      match e.outcome with
      | Expect_type | Expect_ok_or_error _ -> ()
      | _ -> Alcotest.fail "unexpected Type error")
  | Error (Cwl.Error.Schema _) -> (
      match e.outcome with
      | Expect_schema -> ()
      | Expect_ok_or_error _ -> ()
      | _ -> Alcotest.fail "unexpected Schema error")
  | Error (Cwl.Error.Unsupported { feature }) -> (
      match e.outcome with
      | Expect_unsupported want ->
          Alcotest.(check string) "feature" want feature
      | Expect_ok_or_error _ -> ()
      | _ -> Alcotest.fail ("unexpected Unsupported " ^ feature))
  | Error err -> (
      match e.outcome with
      | Expect_ok_or_error _ -> ()
      | _ -> Alcotest.fail (Cwl.Error.to_string err))

let write_tool dir body =
  let path = Filename.concat dir "tool.cwl" in
  Out_channel.with_open_text path (fun oc -> output_string oc body);
  path

let docker_load_saved =
  ( "docker_load_saved",
    `Slow,
    fun () ->
      require_docker ();
      let dir = Filename.temp_dir "ccr-load-" "" in
      let tar = Filename.concat dir "alpine.tar" in
      let bin = Cwl.Runtime.docker_executable () in
      let path_prefix =
        if Filename.is_relative bin then ""
        else "PATH=" ^ Filename.quote (Filename.dirname bin) ^ ":$PATH "
      in
      let q = Filename.quote bin in
      let code =
        Sys.command
          (Printf.sprintf
             "%s%s pull alpine >/dev/null && %s%s save alpine -o %s" path_prefix
             q path_prefix q (Filename.quote tar))
      in
      if code <> 0 then Alcotest.fail "docker save failed";
      let tool =
        write_tool dir
          (Printf.sprintf
             "cwlVersion: v1.2\nclass: CommandLineTool\nrequirements:\n  \
              DockerRequirement:\n    dockerLoad: %s\nbaseCommand: [echo, \
              loaded]\ninputs: []\noutputs: []\n"
             tar)
      in
      let job = Filename.concat dir "job.json" in
      Out_channel.with_open_text job (fun oc -> output_string oc "{}\n");
      Eio_main.run @@ fun env ->
      let local = Cwl.Runtime.local env in
      let docker image = Cwl.Runtime.docker env image in
      match Cwl.run local ~docker tool job with
      | Ok _ -> ()
      | Error e -> Alcotest.fail (Cwl.Error.to_string e) )

let docker_import_saved =
  ( "docker_import_saved",
    `Slow,
    fun () ->
      require_docker ();
      let dir = Filename.temp_dir "ccr-import-" "" in
      let tar = Filename.concat dir "rootfs.tar" in
      let bin = Cwl.Runtime.docker_executable () in
      let path_prefix =
        if Filename.is_relative bin then ""
        else "PATH=" ^ Filename.quote (Filename.dirname bin) ^ ":$PATH "
      in
      let q = Filename.quote bin in
      let code =
        Sys.command
          (Printf.sprintf
             "%s%s rm -f ccr-export >/dev/null 2>&1; %s%s create --name \
              ccr-export alpine true >/dev/null && %s%s export ccr-export -o %s \
              && %s%s rm ccr-export >/dev/null"
             path_prefix q path_prefix q path_prefix q (Filename.quote tar)
             path_prefix q)
      in
      if code <> 0 then Alcotest.fail "docker export failed";
      let gz = tar ^ ".gz" in
      if Sys.command (Printf.sprintf "gzip -c %s > %s" (Filename.quote tar) (Filename.quote gz)) <> 0
      then Alcotest.fail "gzip failed";
      let tool =
        write_tool dir
          (Printf.sprintf
             "cwlVersion: v1.2\nclass: CommandLineTool\nrequirements:\n  \
              DockerRequirement:\n    dockerImport: %s\n    dockerImageId: \
              ccr-import-test\nbaseCommand: [echo, imported]\ninputs: []\noutputs: \
              []\n"
             gz)
      in
      let job = Filename.concat dir "job.json" in
      Out_channel.with_open_text job (fun oc -> output_string oc "{}\n");
      Eio_main.run @@ fun env ->
      let local = Cwl.Runtime.local env in
      let docker image = Cwl.Runtime.docker env image in
      match Cwl.run local ~docker tool job with
      | Ok _ -> ()
      | Error e -> Alcotest.fail (Cwl.Error.to_string e) )

let edge_cases =
  List.map
    (fun e ->
      let speed = if e.docker then `Slow else `Quick in
      (e.name, speed, run_edge e))
    execute_edges

let with_runtime f = Eio_main.run @@ fun env -> f (Cwl.Runtime.local env)

let expect_ok = function
  | Ok v -> v
  | Error e -> Alcotest.fail (Cwl.Error.to_string e)

let planted_symlink_runtime ~prefix ~link_name ~tool ~job () =
  with_runtime @@ fun (module R : Cwl.Runtime.RUNTIME) ->
  let parent = expect_ok (R.mkdtemp prefix) in
  let parent = expect_ok (R.abspath parent) in
  let outdir = Filename.concat parent "out" in
  let outside = Filename.concat parent "secret" in
  expect_ok (R.mkdir_p outdir);
  expect_ok (R.write_file outside "keep\n");
  Unix.symlink outside (Filename.concat outdir link_name);
  match Cwl.run (module R) ~outdir (fixture tool) (fixture job) with
  | Error (Cwl.Error.Runtime _) ->
      Alcotest.(check string)
        "outside unchanged" "keep\n"
        (In_channel.with_open_text outside In_channel.input_all)
  | Error e -> Alcotest.fail ("expected Runtime, got " ^ Cwl.Error.to_string e)
  | Ok _ -> Alcotest.fail "expected Runtime error"

let stdout_symlink_dest =
  ( "stdout_symlink_dest",
    `Quick,
    planted_symlink_runtime ~prefix:"ccr-stdio-" ~link_name:"out.txt"
      ~tool:"echo-stdout.cwl" ~job:"echo-job.json" )

let stdin_symlink_out =
  ( "stdin_symlink_out",
    `Quick,
    planted_symlink_runtime ~prefix:"ccr-stdin-" ~link_name:"in.txt"
      ~tool:"stdin-named.cwl" ~job:"empty-job.json" )

let output_under_outdir =
  ( "output_under_outdir",
    `Quick,
    fun () ->
      Eio_main.run @@ fun env ->
      let (module R : Cwl.Runtime.RUNTIME) = Cwl.Runtime.local env in
      let outdir = expect_ok (R.mkdtemp "ccr-under-") in
      let outdir = expect_ok (R.abspath outdir) in
      match
        Cwl.run
          (module R)
          ~outdir
          (fixture "echo-stdout.cwl")
          (fixture "echo-job.json")
      with
      | Error e -> Alcotest.fail (Cwl.Error.to_string e)
      | Ok ann ->
          let paths =
            List.fold_left (fun acc (_, v) -> collect_paths acc v) [] ann.value
          in
          List.iter (fun p -> expect_ok (R.confined [ outdir ] p)) paths )

let expect_runtime = function
  | Error (Cwl.Error.Runtime _) -> ()
  | Error e ->
      Alcotest.fail ("expected Runtime error, got " ^ Cwl.Error.to_string e)
  | Ok () -> Alcotest.fail "expected Runtime error"

let runtime_setup (module R : Cwl.Runtime.RUNTIME) =
  let parent = expect_ok (R.mkdtemp "ccr-conf-") in
  let parent = expect_ok (R.abspath parent) in
  let root = Filename.concat parent "out" in
  expect_ok (R.mkdir_p root);
  (parent, root)

let confined_inside () =
  with_runtime @@ fun (module R : Cwl.Runtime.RUNTIME) ->
  let _parent, root = runtime_setup (module R) in
  let file = Filename.concat root "a.txt" in
  expect_ok (R.write_file file "x");
  expect_ok (R.confined [ root ] file)

let confined_rejects_sibling () =
  with_runtime @@ fun (module R : Cwl.Runtime.RUNTIME) ->
  let parent, root = runtime_setup (module R) in
  let evil = Filename.concat parent "out-evil" in
  expect_ok (R.mkdir_p evil);
  let file = Filename.concat evil "a.txt" in
  expect_ok (R.write_file file "x");
  expect_runtime (R.confined [ root ] file)

let confined_rejects_dotdot () =
  with_runtime @@ fun (module R : Cwl.Runtime.RUNTIME) ->
  let parent, root = runtime_setup (module R) in
  let other = Filename.concat parent "other" in
  expect_ok (R.mkdir_p other);
  let file = Filename.concat other "a.txt" in
  expect_ok (R.write_file file "x");
  let via_dotdot = Filename.concat root (Filename.concat ".." "other/a.txt") in
  expect_runtime (R.confined [ root ] via_dotdot)

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
    ("dotdot in middle", `Quick, glob_err [ "foo/bar" ] "foo/../bar");
    ("outdir itself", `Quick, glob_ok [ "a.txt" ] "." [ "/out" ]);
    ( "double star",
      `Quick,
      glob_ok
        [ "a.txt"; "dir/b.txt"; "dir/sub/c.txt" ]
        "**/*.txt"
        [ "/out/a.txt"; "/out/dir/b.txt"; "/out/dir/sub/c.txt" ] );
    ("double star depth", `Quick, glob_err [ deep_glob_path 70 ] "**");
    ( "roots exclude outdir",
      `Quick,
      glob_ok ~roots:[ "/elsewhere" ] [ "a.txt" ] "*" [] );
  ]

let () =
  Alcotest.run "cwl"
    [
      ("bind_examples", example_cases);
      ( "bind_properties",
        List.map (QCheck_alcotest.to_alcotest ~speed_level:`Quick) prop_tests );
      ("glob", glob_cases);
      ("runtime", runtime_cases);
      ( "execute",
        edge_cases
        @ [
            stdout_symlink_dest;
            stdin_symlink_out;
            output_under_outdir;
            docker_load_saved;
            docker_import_saved;
          ] );
    ]
