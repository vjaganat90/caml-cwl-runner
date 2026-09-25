(** Docker requirement parsing, mount targets, and the two live image
    acquisitions that need a saved archive. Does not run the execute edge table.
*)

open Harness
open QCheck2

let canonical_container path =
  let segs = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
  segs <> []
  && path = "/" ^ String.concat "/" segs
  && (not (String.contains path ':'))
  && (not (String.exists (fun c -> c < ' ' || c = '\127') path))
  && List.for_all (fun s -> s <> "." && s <> "..") segs

let prop_docker_argv_suffix =
  let workdir =
    let seg =
      Gen.map
        (fun s -> "d" ^ s)
        Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 0 6))
    in
    Gen.map
      (fun segs -> "/" ^ String.concat "/" segs)
      Gen.(list_size (int_range 1 4) seg)
  in
  Test.make ~name:"docker run keeps the CWL argv as a suffix" ~count:40
    Gen.(
      pair
        (list_size (int_range 1 6) (string_size ~gen:char (int_range 1 8)))
        workdir)
    (fun (raw, workdir) ->
      let argv = List.map (fun s -> "c" ^ s) raw in
      let spec =
        {
          Cwl.Runtime.bin = "/bin/docker";
          user = "1:1";
          cwd = "/host-out";
          workdir;
          image = "alpine";
        }
      in
      let got = Cwl.Runtime.docker_run_argv spec argv in
      let n = List.length argv in
      let pre_len = List.length got - n in
      let flag name =
        let rec go = function
          | f :: v :: _ when f = name -> Some v
          | _ :: rest -> go rest
          | [] -> None
        in
        go got
      in
      let vol = spec.cwd ^ ":" ^ spec.workdir in
      pre_len > 0
      && List.drop pre_len got = argv
      && List.nth got (pre_len - 1) = spec.image
      && flag "-w" = Some spec.workdir
      && flag "-v" = Some vol
      && String.split_on_char ':' vol = [ spec.cwd; spec.workdir ])

let prop_docker_diagnosed =
  Test.make ~name:"unimplemented keys are diagnosed" ~count:1 Gen.unit
    (fun () ->
      match Cwl.Untyped_tree.load_file (fixture "docker-diagnosed.cwl") with
      | Error _ -> false
      | Ok tree -> (
          match Cwl.Command_line_tool.of_tree tree with
          | Error _ -> false
          | Ok ann ->
              has_feature "DockerRequirement" ann.diagnostics
              && List.for_all
                   (fun f -> not (has_feature f ann.diagnostics))
                   [ "stdout"; "outputs" ]))

let prop_container_outdir =
  let legal_path =
    let seg =
      Gen.map
        (fun s -> "d" ^ s)
        Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 0 6))
    in
    Gen.map
      (fun segs -> "/" ^ String.concat "/" segs)
      Gen.(list_size (int_range 1 5) seg)
  in
  Test.make ~name:"container outdir is a canonical absolute path" ~count:200
    (Gen.oneof [ legal_path; Gen.string ])
    (fun path ->
      match Cwl.Schema.Container_outdir.of_string path with
      | Ok p ->
          canonical_container path
          && Cwl.Schema.Container_outdir.to_string p = path
      | Error _ -> not (canonical_container path))

let container_outdir_cases =
  [
    ("/other", true);
    ("/var/spool/cwl", true);
    ("/out/.hidden", true);
    ("/out/..hidden", true);
    ("/out/my dir", true);
    ("/tmp", true);
    ("/a", true);
    ("/Out", true);
    ("/out/目录", true);
    ("/out\\x", true);
    ("", false);
    ("out", false);
    ("./out", false);
    ("../etc", false);
    ("/", false);
    ("//", false);
    ("///", false);
    ("//out", false);
    ("/out/", false);
    ("/out//x", false);
    ("/out/../etc", false);
    ("/out/../out", false);
    ("/./out", false);
    ("/out/.", false);
    ("/out/..", false);
    ("/out:rw", false);
    (":/out", false);
    ("/out/./x", false);
    ("/out\n", false);
    ("/out/foo/../../etc", false);
  ]

let container_outdir_table () =
  List.iter
    (fun (path, legal) ->
      if canonical_container path <> legal then
        Alcotest.failf "predicate disagrees with %S" path;
      match Cwl.Schema.Container_outdir.of_string path with
      | Ok got when legal ->
          Alcotest.(check string)
            path path
            (Cwl.Schema.Container_outdir.to_string got)
      | Ok _ -> Alcotest.failf "accepted %S" path
      | Error _ when not legal -> ()
      | Error message -> Alcotest.failf "rejected %S (%s)" path message)
    container_outdir_cases

let parse_src src =
  match Cwl.Untyped_tree.load_string src with
  | Error _ as e -> e
  | Ok tree -> Cwl.Command_line_tool.of_tree tree

let parse_fixture name = parse_src (read_fixture name)

let docker_field reqs =
  match
    List.find_map
      (function Cwl.Schema.Docker d -> Some d.output_directory | _ -> None)
      reqs
  with
  | None -> None
  | Some path -> Some (Option.map Cwl.Schema.Container_outdir.to_string path)

let docker_output_documents () =
  let cases =
    [
      ("absent", "docker-outdir/absent.cwl", `Field None);
      ("set", "docker-outdir/set.cwl", `Field (Some "/other"));
      ("int", "docker-outdir/int.cwl", `Schema);
      ("null", "docker-outdir/null.cwl", `Schema);
      ("list", "docker-outdir/list.cwl", `Schema);
      ("bool", "docker-outdir/bool.cwl", `Schema);
      ("empty", "docker-outdir/empty.cwl", `Schema);
      ("hint", "docker-outdir/hint.cwl", `Hint "/other");
    ]
  in
  List.iter
    (fun (name, file, expect) ->
      match parse_fixture file with
      | Error (Cwl.Error.Schema _) ->
          if expect <> `Schema then
            Alcotest.failf "%s: unexpected schema error" name
      | Error e -> Alcotest.failf "%s: %s" name (Cwl.Error.to_string e)
      | Ok ann -> (
          match expect with
          | `Schema -> Alcotest.failf "%s: expected schema error" name
          | `Field want ->
              Alcotest.(check (option (option string)))
                name (Some want)
                (docker_field ann.value.requirements)
          | `Hint path ->
              Alcotest.(check (option (option string)))
                "requirements" None
                (docker_field ann.value.requirements);
              Alcotest.(check (option (option string)))
                name (Some (Some path))
                (docker_field ann.value.hints);
              Alcotest.(check bool)
                "hint diagnosed" true
                (has_feature "DockerRequirement" ann.diagnostics)))
    cases;
  match parse_fixture "docker-outdir/duplicate.cwl" with
  | Error (Cwl.Error.Schema _) -> ()
  | Error e -> Alcotest.fail (Cwl.Error.to_string e)
  | Ok _ -> Alcotest.fail "repeated DockerRequirement was accepted"

let json_quote s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let yaml_quote s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

type designated =
  | Json of string
  | Glob_file of string
  | Glob_dir of string
  | Host_outdir
  | Input_container
  | Input_host

let designated_text = function
  | Host_outdir -> read_fixture "designated/host.cwl"
  | Json _ -> read_fixture "designated/json.cwl"
  | Glob_file pat ->
      substitute
        (read_fixture "designated/glob-file.cwl")
        "@GLOB@" (yaml_quote pat)
  | Glob_dir pat ->
      substitute
        (read_fixture "designated/glob-dir.cwl")
        "@GLOB@" (yaml_quote pat)
  | Input_container -> read_fixture "designated/input-container.cwl"
  | Input_host -> read_fixture "designated/input-host.cwl"

let designated_cases =
  [
    ("json under", Json "/other/thing", true);
    ("json dot segment", Json "/other/./thing", true);
    ("json trailing slash", Json "/other/", false);
    ("json sibling prefix", Json "/other-evil/thing", false);
    ("json dotdot", Json "/other/../etc", false);
    ("json dotdot back", Json "/other/thing/../thing", false);
    ("json outside", Json "/etc/passwd", false);
    ("json relative", Json "thing", true);
    ("json relative escape", Json "../evil", false);
    ("glob under", Glob_file "/other/thing", true);
    ("glob dot segment", Glob_file "/other/./thing", true);
    ("glob outside", Glob_file "/etc/passwd", false);
    ("glob dotdot", Glob_file "/other/../etc", false);
    ("glob outdir", Glob_dir "$(runtime.outdir)", true);
    ("glob dot dir", Glob_dir "/other/.", true);
    ("glob trimmed", Glob_file " /other/thing", true);
    ("glob trimmed outside", Glob_file " /etc/passwd", false);
    ("absent is host outdir", Host_outdir, true);
    ("staged location is container path", Input_container, true);
    ("staged location stays host path", Input_host, true);
  ]

let docker_mount_table () =
  let parent = Filename.temp_dir "ccr-mnt-" "" in
  let real = Filename.concat parent "real" in
  let link = Filename.concat parent "link" in
  Unix.mkdir real 0o755;
  Unix.symlink real link;
  let real = Unix.realpath real in
  let check_ok ~host ~workdir ~source ~target =
    match Cwl.Runtime.docker_mount ~host ~workdir with
    | Ok (got_source, got_target) ->
        Alcotest.(check string) "source" source got_source;
        Alcotest.(check string) "target" target got_target
    | Error e -> Alcotest.fail (Cwl.Error.to_string e)
  in
  check_ok ~host:real ~workdir:"/other" ~source:real ~target:"/other";
  check_ok ~host:link ~workdir:link ~source:real ~target:link;
  List.iter
    (fun workdir ->
      match Cwl.Runtime.docker_mount ~host:real ~workdir with
      | Error (Cwl.Error.Runtime _) -> ()
      | Ok (src, dst) ->
          Alcotest.failf "accepted mount %S -> %S:%S" workdir src dst
      | Error e -> Alcotest.fail (Cwl.Error.to_string e))
    [ "/other/../etc"; "/"; "relative"; "/out:rw"; "/out/./x"; "" ]

let designated_outdir_table () =
  List.iter
    (fun (name, case, expect_ok) ->
      let dir = Filename.temp_dir "ccr-dod-" "" in
      let tool =
        write_text (Filename.concat dir "tool.cwl") (designated_text case)
      in
      let job = Filename.concat dir "job.json" in
      let job_body =
        match case with
        | Input_container | Input_host ->
            Out_channel.with_open_text (Filename.concat dir "hello.txt")
              (fun oc -> output_string oc "hi\n");
            "{\"f\":{\"class\":\"File\",\"location\":\"hello.txt\"}}\n"
        | _ -> "{}\n"
      in
      Out_channel.with_open_text job (fun oc -> output_string oc job_body);
      let argv_ok = ref false in
      let result =
        Eio_main.run @@ fun env ->
        let (module Local : Cwl.Runtime.RUNTIME) = Cwl.Runtime.local env in
        let module Local = (val (module Local) : Cwl.Runtime.RUNTIME) in
        let module R = struct
          include Local

          let spawn ~env:_ cwd _stdio argv =
            (argv_ok :=
               match case with
               | Host_outdir -> List.mem cwd argv
               | Input_host -> List.mem (Filename.concat cwd "hello.txt") argv
               | Input_container ->
                   List.mem "/other/hello.txt" argv
                   && not (List.mem (Filename.concat cwd "hello.txt") argv)
               | _ -> List.mem "/other" argv);
            match write_file (Filename.concat cwd "thing") "ok\n" with
            | Error _ as e -> e
            | Ok () -> (
                let json_path =
                  match case with
                  | Json path -> Some path
                  | Host_outdir -> Some "thing"
                  | Glob_file _ | Glob_dir _ | Input_container | Input_host ->
                      None
                in
                match json_path with
                | None -> Ok 0
                | Some path -> (
                    let body =
                      Printf.sprintf
                        "{\"f\":{\"class\":\"File\",\"path\":%s}}\n"
                        (json_quote path)
                    in
                    match
                      write_file (Filename.concat cwd "cwl.output.json") body
                    with
                    | Ok () -> Ok 0
                    | Error _ as e -> e))
        end in
        Cwl.run
          (module Local)
          ~docker:(fun _req -> (module R : Cwl.Runtime.RUNTIME))
          tool job
      in
      if not !argv_ok then
        Alcotest.failf "%s: $(runtime.outdir) was not the designated directory"
          name
      else
        match (result, expect_ok) with
        | Ok ann, true -> (
            match case with
            | Input_container | Input_host -> ()
            | Glob_dir _ -> (
                match Cwl.Type.lookup "d" ann.value with
                | Some (Cwl.Type.Vdir d) -> (
                    match Cwl.Type.dir_path d with
                    | Some p ->
                        Alcotest.(check bool) name true (Sys.is_directory p);
                        Alcotest.(check bool) name false (p = "/other")
                    | None -> Alcotest.failf "%s: directory missing path" name)
                | _ -> Alcotest.failf "%s: expected Directory" name)
            | _ -> (
                lookup_file_basename "f" "thing" ann;
                lookup_bytes "f" "ok\n" ann;
                match lookup_file_path "f" ann with
                | Some p ->
                    Alcotest.(check bool)
                      name false
                      (String.starts_with ~prefix:"/other" p)
                | None -> Alcotest.failf "%s: file missing path" name))
        | Ok _, false -> Alcotest.failf "%s: expected failure" name
        | Error (Cwl.Error.Runtime _ | Cwl.Error.Type _), false -> ()
        | Error e, _ -> Alcotest.failf "%s: %s" name (Cwl.Error.to_string e))
    designated_cases

let docker_prefix () =
  let bin = Cwl.Runtime.docker_executable () in
  let q = Filename.quote bin in
  let prefix =
    if Filename.is_relative bin then ""
    else "PATH=" ^ Filename.quote (Filename.dirname bin) ^ ":$PATH "
  in
  (prefix, q)

let run_archive_tool ~dir ~fixture_name ~archive () =
  let tool =
    write_text
      (Filename.concat dir "tool.cwl")
      (substitute (read_fixture fixture_name) "@ARCHIVE@" archive)
  in
  let job = write_text (Filename.concat dir "job.json") "{}\n" in
  Eio_main.run @@ fun env ->
  let local = Cwl.Runtime.local env in
  let docker image = Cwl.Runtime.docker env image in
  match Cwl.run local ~docker tool job with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Cwl.Error.to_string e)

let docker_load_saved =
  ( "docker_load_saved",
    `Slow,
    fun () ->
      require_docker ();
      let dir = Filename.temp_dir "ccr-load-" "" in
      let tar = Filename.concat dir "alpine.tar" in
      let prefix, q = docker_prefix () in
      let code =
        Sys.command
          (Printf.sprintf
             "%s%s pull alpine >/dev/null && %s%s save alpine -o %s" prefix q
             prefix q (Filename.quote tar))
      in
      if code <> 0 then Alcotest.fail "docker save failed";
      run_archive_tool ~dir ~fixture_name:"docker-load.cwl" ~archive:tar () )

let docker_import_saved =
  ( "docker_import_saved",
    `Slow,
    fun () ->
      require_docker ();
      let dir = Filename.temp_dir "ccr-import-" "" in
      let tar = Filename.concat dir "rootfs.tar" in
      let prefix, q = docker_prefix () in
      let code =
        Sys.command
          (Printf.sprintf
             "%s%s rm -f ccr-export >/dev/null 2>&1; %s%s create --name \
              ccr-export alpine true >/dev/null && %s%s export ccr-export -o \
              %s && %s%s rm ccr-export >/dev/null"
             prefix q prefix q prefix q (Filename.quote tar) prefix q)
      in
      if code <> 0 then Alcotest.fail "docker export failed";
      let gz = tar ^ ".gz" in
      if
        Sys.command
          (Printf.sprintf "gzip -c %s > %s" (Filename.quote tar)
             (Filename.quote gz))
        <> 0
      then Alcotest.fail "gzip failed";
      run_archive_tool ~dir ~fixture_name:"docker-import.cwl" ~archive:gz () )

let tests =
  [
    ( "docker_properties",
      List.map
        (QCheck_alcotest.to_alcotest ~speed_level:`Quick)
        [
          prop_docker_argv_suffix; prop_docker_diagnosed; prop_container_outdir;
        ] );
    ( "docker_outdir",
      [
        ("paths", `Quick, container_outdir_table);
        ("documents", `Quick, docker_output_documents);
        ("mount", `Quick, docker_mount_table);
        ("designated", `Quick, designated_outdir_table);
      ] );
    ("docker_image", [ docker_load_saved; docker_import_saved ]);
  ]
