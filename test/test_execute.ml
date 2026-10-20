(** Execute edges over fixture files, plus the symlink confinement cases. Does
    not build argv properties or parse documents on its own. *)

open Harness

let run_tool tool job =
  Eio_main.run @@ fun env ->
  Cwl.run (Runtime.local env) (fixture tool) (Some (fixture job))

type outcome =
  | Expect_ok of (Cwl.Type.object_ Cwl.Error.annotated -> unit)
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

let collect_paths acc v =
  Cwl.Type.fold
    (fun acc v ->
      Option.fold ~none:acc ~some:(fun p -> p :: acc) (Cwl.Type.path v))
    acc v

(* The basename and checksum of each entry in the [listing] of output [id]. *)
let listing id ann =
  match Option.bind (lookup id ann) (Cwl.Type.get "listing") with
  | Some (Cwl.Untyped_tree.Array entries) ->
      List.map
        (fun e ->
          (Cwl.Type.get_string "basename" e, Cwl.Type.get_string "checksum" e))
        entries
  | _ -> []

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
    edge ~docker:true "docker_output_directory" "docker-output-dir.cwl"
      (Expect_ok
         (fun ann ->
           lookup_bytes "where" "/other" ann;
           lookup_file_basename "thing" "thing" ann));
    edge ~docker:true "docker_echo" "docker-req.cwl" (Expect_ok (fun _ -> ()));
    edge ~docker:true "docker_image_id" "docker-image-id.cwl"
      (Expect_ok (fun _ -> ()));
    edge ~docker:true "docker_json_escape" "docker-json-escape.cwl"
      Expect_runtime;
    edge "output_eval_no_json" "output-eval.cwl"
      (Expect_unsupported "outputEval");
    edge "output_eval_no_glob" "output-eval-no-glob.cwl"
      (Expect_ok (lookup_int "n" 3L));
    edge "json_ignores_output_eval" "json-and-eval.cwl"
      (Expect_ok (lookup_int "n" 1L));
    edge "stdin_cat" "stdin-cat.cwl" ~job:"stdin-job.json"
      (Expect_ok (lookup_bytes "out" "hello from file\n"));
    edge "stdin_cat_percent" "stdin-cat.cwl" ~job:"percent-job.json"
      (Expect_ok (lookup_bytes "out" "hello from file\n"));
    edge "stdin_cat_literal" "stdin-cat.cwl" ~job:"literal-job.json"
      (Expect_ok (lookup_bytes "out" "literal\n"));
    edge "default_tool_relative" "default-loc/tool.cwl"
      (Expect_ok (lookup_bytes "out" "hello from tool dir\n"));
    edge "directory_from_tool_cwd" "dir-ls.cwl" ~job:"dir-in-job.json"
      (Expect_ok (fun _ -> ()));
    edge "undeclared_file_ignored" "file-in.cwl" ~job:"undeclared-job.json"
      (Expect_ok (fun _ -> ()));
    edge "stderr_stream" "stderr-stream.cwl"
      (Expect_ok (lookup_bytes "err" "err\n"));
    edge "dir_glob" "dir-glob.cwl"
      (Expect_ok
         (fun ann ->
           lookup_dir "d" ann;
           Alcotest.(check (list (pair (option string) (option string))))
             "listing"
             [
               (Some "f", Some "sha1$6fcf9dfbd479ed82697fee719b9f8c610a11ff2a");
             ]
             (listing "d" ann)));
    edge "dir_listing_symlink_out" "dir-symlink.cwl" Expect_runtime;
    edge "dir_literal_file_literal" "dir-literal.cwl"
      ~job:"dir-literal-job.json"
      (Expect_ok (lookup_bytes "out" "hi\n"));
    edge "stdin_in_subdir" "stdin-dir.cwl" ~job:"dir-literal-job.json"
      (Expect_ok (lookup_bytes "out" "hi\n"));
    edge "dir_literal_located_file" "dir-literal.cwl"
      ~job:"dir-located-job.json"
      (Expect_ok (lookup_bytes "out" "hello from file\n"));
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
             "nameroot" (Some "foo")
             (Cwl.Type.get_string "nameroot" f);
           Alcotest.(check (option string))
             "nameext" (Some ".txt")
             (Cwl.Type.get_string "nameext" f);
           let json = Cwl.Type.to_json ~sorted:false f in
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
           match Option.map (classed "Directory" "d") (lookup "d" ann) with
           | Some v -> (
               match Cwl.Type.path v with
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
                 (not (String.starts_with ~prefix:"/etc/" p));
               Alcotest.(check (option string))
                 "format kept" (Some "http://edamontology.org/format_2330")
                 (Cwl.Type.get_string "format" (lookup_file "f" ann))
           | None -> Alcotest.fail "File missing path"));
    edge "json_extra_int_kept" "json-extra-int.cwl"
      (Expect_ok (lookup_int "n" 1L));
    edge "nameroot_no_dot" "nameroot-plain.cwl"
      (Expect_ok
         (fun ann ->
           let f = lookup_file "f" ann in
           Alcotest.(check (option string))
             "nameroot" (Some "foo")
             (Cwl.Type.get_string "nameroot" f);
           Alcotest.(check (option string))
             "nameext" (Some "")
             (Cwl.Type.get_string "nameext" f)));
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
           match List.assoc_opt "out" ann.value with
           | Some v ->
               let p = String.trim (file_bytes v) in
               Alcotest.(check bool) "is dir" true (Sys.is_directory p)
           | None -> Alcotest.fail "missing out"));
    edge "glob_overlap_uniq" "glob-overlap.cwl"
      (Expect_ok (lookup_basenames "files" [ "a.txt" ]));
    edge "json_file_is_dir" "json-file-is-dir.cwl" Expect_type;
    edge "workflow_unsupported" "workflow.cwl" (Expect_unsupported "Workflow");
    edge "glob_starstar_root" "glob-starstar.cwl" Expect_runtime;
  ]

let run_edge e () =
  if e.docker then require_docker ();
  let result =
    if not e.docker then run_tool e.tool e.job
    else
      Eio_main.run @@ fun env ->
      let local = Runtime.local env in
      let docker ~network image = Runtime.docker env ~network image in
      Cwl.run local ~docker (fixture e.tool) (Some (fixture e.job))
  in
  match result with
  | Ok ann -> (
      match e.outcome with
      | Expect_ok f -> f ann
      | Expect_runtime -> Alcotest.fail "expected Runtime error"
      | Expect_type -> Alcotest.fail "expected Type error"
      | Expect_schema -> Alcotest.fail "expected Schema error"
      | Expect_unsupported _ -> Alcotest.fail "expected Unsupported")
  | Error (Cwl.Error.Runtime _ as err) -> (
      match e.outcome with
      | Expect_runtime -> ()
      | _ ->
          Alcotest.fail ("unexpected Runtime error: " ^ Cwl.Error.to_string err)
      )
  | Error (Cwl.Error.Type _) -> (
      match e.outcome with
      | Expect_type -> ()
      | _ -> Alcotest.fail "unexpected Type error")
  | Error (Cwl.Error.Schema _) -> (
      match e.outcome with
      | Expect_schema -> ()
      | _ -> Alcotest.fail "unexpected Schema error")
  | Error (Cwl.Error.Unsupported { feature }) -> (
      match e.outcome with
      | Expect_unsupported want ->
          Alcotest.(check string) "feature" want feature
      | _ -> Alcotest.fail ("unexpected Unsupported " ^ feature))
  | Error err -> Alcotest.fail (Cwl.Error.to_string err)

let edge_cases =
  List.map
    (fun e ->
      let speed = if e.docker then `Slow else `Quick in
      (e.name, speed, run_edge e))
    execute_edges

let planted_symlink_runtime ~prefix ~link_name ~tool ~job () =
  with_runtime @@ fun (module R) ->
  let parent = expect_ok (R.mkdtemp prefix) in
  let parent = expect_ok (R.abspath parent) in
  let outdir = Filename.concat parent "out" in
  let outside = Filename.concat parent "secret" in
  expect_ok (R.mkdir_p outdir);
  expect_ok (R.write_file outside "keep\n");
  Unix.symlink outside (Filename.concat outdir link_name);
  match Cwl.run (module R) ~outdir (fixture tool) (Some (fixture job)) with
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
      let (module R) = Runtime.local env in
      let outdir = expect_ok (R.mkdtemp "ccr-under-") in
      let outdir = expect_ok (R.abspath outdir) in
      match
        Cwl.run
          (module R)
          ~outdir
          (fixture "echo-stdout.cwl")
          (Some (fixture "echo-job.json"))
      with
      | Error e -> Alcotest.fail (Cwl.Error.to_string e)
      | Ok ann ->
          let paths =
            List.fold_left (fun acc (_, v) -> collect_paths acc v) [] ann.value
          in
          List.iter (fun p -> expect_ok (R.confined [ outdir ] p)) paths )

let tests =
  [
    ( "execute",
      edge_cases
      @ [ stdout_symlink_dest; stdin_symlink_out; output_under_outdir ] );
  ]
