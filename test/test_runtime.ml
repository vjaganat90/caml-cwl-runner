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

      let spawn env cwd _stdio _argv =
        seen := Cwl.Runtime.env_list env;
        cwd_seen := cwd;
        Ok 0
    end in
    Cwl.run (module R) tool (Some job)
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
  Alcotest.(check (list string)) "names" [ "HOME"; "TMPDIR" ] names;
  Alcotest.(check bool)
    "HOME is cwd" true
    (List.mem ("HOME=" ^ !cwd_seen) !seen);
  Alcotest.(check bool)
    "TMPDIR differs" true
    (not (List.mem ("TMPDIR=" ^ !cwd_seen) !seen));
  match Sys.getenv_opt "HOME" with
  | None -> ()
  | Some home when home = !cwd_seen -> ()
  | Some home ->
      Alcotest.(check bool)
        "parent HOME absent" false
        (List.mem ("HOME=" ^ home) !seen)

(* Cwl.run reads the tool and the job through the runtime it is given, so a
   fake sees every document read. *)
let documents_load_through_runtime () =
  let dir = Filename.temp_dir "ccr-load-" "" in
  let tool = copy_fixture dir "true.cwl" "tool.cwl" in
  let job = Filename.concat dir "job.json" in
  Out_channel.with_open_text job (fun oc -> output_string oc "{}\n");
  let read = ref [] in
  let result =
    Eio_main.run @@ fun env ->
    let (module Local : Cwl.Runtime.RUNTIME) = Cwl.Runtime.local env in
    let module R = struct
      include Local

      let read_file path =
        read := path :: !read;
        Local.read_file path
    end in
    Cwl.run (module R) tool (Some job)
  in
  (match result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Cwl.Error.to_string e));
  Alcotest.(check (list string)) "documents read" [ tool; job ] (List.rev !read)

(* A runtime call made in a cancelled fiber stops the fiber. It must not
   come back as an [Error] the caller could carry on from. *)
let cancellation_passes_through () =
  with_runtime @@ fun (module R : Cwl.Runtime.RUNTIME) ->
  let _, root = runtime_setup (module R) in
  let file = Filename.concat root "a.txt" in
  expect_ok (R.write_file file "x");
  match
    Eio.Cancel.sub (fun context ->
        Eio.Cancel.cancel context Exit;
        R.read_file file)
  with
  | exception Eio.Cancel.Cancelled Exit -> ()
  | Ok _ -> Alcotest.fail "read_file ran in a cancelled fiber"
  | Error e -> Alcotest.failf "cancellation became %s" (Cwl.Error.to_string e)

(* The local launcher's real process: nothing but HOME, TMPDIR, and the
   parent's PATH. *)
let local_env_inherits_only_path () =
  let dir = Filename.temp_dir "ccr-env-" "" in
  let out = Filename.concat dir "env.txt" in
  let code =
    Eio_main.run @@ fun env ->
    let (module R : Cwl.Runtime.RUNTIME) = Cwl.Runtime.local env in
    R.spawn
      { home = dir; tmpdir = dir }
      dir
      { stdin_file = None; stdout_file = Some out; stderr_file = None }
      [ "/usr/bin/env" ]
  in
  Alcotest.(check (result int reject)) "env ran" (Ok 0) code;
  let seen =
    In_channel.with_open_text out In_channel.input_lines
    |> List.sort String.compare
  in
  let path =
    match Sys.getenv_opt "PATH" with None -> [] | Some p -> [ "PATH=" ^ p ]
  in
  Alcotest.(check (list string))
    "environment"
    (List.sort String.compare ([ "HOME=" ^ dir; "TMPDIR=" ^ dir ] @ path))
    seen

let no_job_uses_defaults () =
  match
    Eio_main.run @@ fun env ->
    Cwl.run (Cwl.Runtime.local env) (fixture "no-job-default.cwl") None
  with
  | Error e -> Alcotest.fail (Cwl.Error.to_string e)
  | Ok ann -> lookup_bytes "out" "from-default\n" ann

let contains ~sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let uncaptured_output_reaches_console () =
  let buf = Buffer.create 64 in
  let result =
    Eio_main.run @@ fun env ->
    let console = (Eio.Flow.buffer_sink buf :> Cwl.Runtime.console) in
    Cwl.run (Cwl.Runtime.local ~console env) (fixture "console.cwl") None
  in
  (match result with
  | Error (Cwl.Error.Runtime _) -> ()
  | Error e -> Alcotest.fail (Cwl.Error.to_string e)
  | Ok _ -> Alcotest.fail "exit 3 is not a success code");
  let seen = Buffer.contents buf in
  Alcotest.(check bool) "stdout" true (contains ~sub:"to-out" seen);
  Alcotest.(check bool) "stderr" true (contains ~sub:"to-err" seen)

let sha1_vectors () =
  with_runtime @@ fun (module R : Cwl.Runtime.RUNTIME) ->
  let dir = expect_ok (R.mkdtemp "ccr-sha1-") in
  List.iter
    (fun (name, body, hex) ->
      let file = Filename.concat dir name in
      expect_ok (R.write_file file body);
      Alcotest.(check string) name hex (expect_ok (R.sha1 file)))
    [
      ("empty", "", "da39a3ee5e6b4b0d3255bfef95601890afd80709");
      ("abc", "abc", "a9993e364706816aba3e25717850c26c9cd0d89d");
      ( "million-a",
        String.make 1_000_000 'a',
        "34aa973cd4c4daa4f61eeb2bdbad27316534016f" );
    ]

let output_file_checksum () =
  match
    Eio_main.run @@ fun env ->
    Cwl.run (Cwl.Runtime.local env) (fixture "no-job-default.cwl") None
  with
  | Error e -> Alcotest.fail (Cwl.Error.to_string e)
  | Ok ann ->
      let f = lookup_file "out" ann in
      let sum = "sha1$e2fcc2f00a193284d3bcd74d7dfc209f05899362" in
      Alcotest.(check (option string)) "checksum" (Some sum) f.checksum;
      Alcotest.(check bool)
        "in JSON" true
        (contains
           ~sub:(Printf.sprintf "\"checksum\":%S" sum)
           (Cwl.Type.object_to_json ann.value))

(* Runs tmpdir-report.cwl in a fresh outdir. Returns the run result, the
   tool's TMPDIR, and the outdir. *)
let tmpdir_run ?rm_tmpdir job =
  let outdir = Filename.temp_dir "ccr-rm-" "" in
  let result =
    Eio_main.run @@ fun env ->
    Cwl.run (Cwl.Runtime.local env) ~outdir ?rm_tmpdir
      (fixture "tmpdir-report.cwl")
      (Some (fixture job))
  in
  let tmpdir =
    In_channel.with_open_text
      (Filename.concat outdir "tmpdir.txt")
      In_channel.input_all
  in
  (result, tmpdir, outdir)

let rm_tmpdir_on_success () =
  let result, tmpdir, outdir = tmpdir_run "tmpdir-ok.json" in
  (match result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Cwl.Error.to_string e));
  Alcotest.(check bool) "tmpdir removed" false (Sys.file_exists tmpdir);
  Alcotest.(check bool)
    "symlink target kept" true
    (Sys.file_exists (Filename.concat outdir "tmpdir.txt"))

let rm_tmpdir_on_failure () =
  let result, tmpdir, _ = tmpdir_run "tmpdir-fail.json" in
  (match result with
  | Error (Cwl.Error.Runtime _) -> ()
  | Error e -> Alcotest.fail (Cwl.Error.to_string e)
  | Ok _ -> Alcotest.fail "exit 3 is not a success code");
  Alcotest.(check bool) "tmpdir removed" false (Sys.file_exists tmpdir)

let leave_tmpdir () =
  let result, tmpdir, _ = tmpdir_run ~rm_tmpdir:false "tmpdir-ok.json" in
  (match result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Cwl.Error.to_string e));
  Alcotest.(check bool)
    "tmpdir kept" true
    (Sys.file_exists (Filename.concat tmpdir "scratch"));
  ignore (Sys.command ("rm -rf " ^ Filename.quote tmpdir) : int)

let tests =
  [
    ( "runtime",
      [
        ("confined_inside", `Quick, confined_inside);
        ("confined_rejects_sibling", `Quick, confined_rejects_sibling);
        ("confined_rejects_dotdot", `Quick, confined_rejects_dotdot);
        ("process_env", `Quick, process_env_table);
        ("cancellation_passes_through", `Quick, cancellation_passes_through);
        ( "documents_load_through_runtime",
          `Quick,
          documents_load_through_runtime );
        ("local_env_inherits_only_path", `Quick, local_env_inherits_only_path);
        ("no_job_uses_defaults", `Quick, no_job_uses_defaults);
        ("uncaptured_output", `Quick, uncaptured_output_reaches_console);
        ("sha1_vectors", `Quick, sha1_vectors);
        ("output_file_checksum", `Quick, output_file_checksum);
        ("rm_tmpdir_on_success", `Quick, rm_tmpdir_on_success);
        ("rm_tmpdir_on_failure", `Quick, rm_tmpdir_on_failure);
        ("leave_tmpdir", `Quick, leave_tmpdir);
      ] );
  ]
