(** Local runtime confinement and the tool process environment. Does not parse a
    CommandLineTool beyond the true-tool fixture. *)

open Harness

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

let process_env_table () =
  let dir = Filename.temp_dir "ccr-env-" "" in
  let tool = copy_fixture dir "true.cwl" "tool.cwl" in
  let job = Filename.concat dir "job.json" in
  Out_channel.with_open_text job (fun oc -> output_string oc "{}\n");
  let seen = ref [] in
  let cwd_seen = ref "" in
  let result =
    Eio_main.run @@ fun env ->
    let (module Local : Cwl.Runtime.RUNTIME) = Cwl.Runtime.local env in
    let module R = struct
      include Local

      let spawn ~env cwd _stdio _argv =
        seen := env;
        cwd_seen := cwd;
        Ok 0
    end in
    Cwl.run (module R) tool job
  in
  (match result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Cwl.Error.to_string e));
  let names =
    List.map
      (fun e ->
        match String.index_opt e '=' with
        | None -> e
        | Some i -> String.sub e 0 i)
      !seen
  in
  let path = Sys.getenv_opt "PATH" in
  let expect_names =
    match path with
    | None -> [ "HOME"; "TMPDIR" ]
    | Some _ -> [ "HOME"; "TMPDIR"; "PATH" ]
  in
  Alcotest.(check (list string)) "names" expect_names names;
  Alcotest.(check bool)
    "HOME is cwd" true
    (List.mem ("HOME=" ^ !cwd_seen) !seen);
  Alcotest.(check bool)
    "TMPDIR differs" true
    (not (List.mem ("TMPDIR=" ^ !cwd_seen) !seen));
  (match path with
  | None -> ()
  | Some p ->
      Alcotest.(check bool) "PATH copied" true (List.mem ("PATH=" ^ p) !seen));
  match Sys.getenv_opt "HOME" with
  | None -> ()
  | Some home when home = !cwd_seen -> ()
  | Some home ->
      Alcotest.(check bool)
        "parent HOME absent" false
        (List.mem ("HOME=" ^ home) !seen)

let tests =
  [
    ( "runtime",
      [
        ("confined_inside", `Quick, confined_inside);
        ("confined_rejects_sibling", `Quick, confined_rejects_sibling);
        ("confined_rejects_dotdot", `Quick, confined_rejects_dotdot);
        ("process_env", `Quick, process_env_table);
      ] );
  ]
