(** In-memory glob patterns. Does not spawn and does not read fixture files. *)

open Harness

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

let tests =
  [
    ( "glob",
      [
        ("literal", `Quick, glob_ok [ "a.txt" ] "a.txt" [ "/out/a.txt" ]);
        ( "star",
          `Quick,
          glob_ok
            [ "a.txt"; "b.txt"; "c.dat" ]
            "*.txt"
            [ "/out/a.txt"; "/out/b.txt" ] );
        ( "question",
          `Quick,
          glob_ok [ "a.txt"; "ab.txt" ] "?.txt" [ "/out/a.txt" ] );
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
        ( "hidden not matched",
          `Quick,
          glob_ok [ ".foo"; "bar" ] "*" [ "/out/bar" ] );
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
      ] );
  ]
