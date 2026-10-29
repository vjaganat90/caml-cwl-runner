(** Tests of the gate: every check catches what it should and passes what it should, on commits made in a throwaway git
    repository. A fake [dune] stands in for the real one where a check builds or runs tests. *)

open Gate

let config =
  {|{"budgets": {"feat": {"code": 150, "tests": 60}, "fix": {"code": 40, "tests": 40}, "test": {"code": 0, "tests": 40},
     "refactor": {"code": 0, "tests": 0}, "other": {"code": 0, "tests": 0}},
     "window": 3, "max_function": 3, "top_level": ["bin", "gates.json", "lib", "test"]}|}

let mutant ?(file = "lib/a.ml") ?(old = "x = x") ?(fresh = "x = 0") ?(why = "f is the identity") () =
  Printf.sprintf {|{"file": %S, "old": %S, "new": %S, "why": %S}|} file old fresh why

let ( => ) path text = (path, Some text)
let rep n line = String.concat "" (List.init n (Fun.const line))
let sub a b s = Re.replace_string (Re.compile (Re.str a)) ~by:b s
let t_ml = "(** Tests of A. *)\n\nlet name = \"f is the identity\"\nlet quote = '\"'\nlet () = assert (A.f 1 = 1)\n"
let a_mli = "(** A. *)\n\nval f : int -> int\n(** [f x] is [x] itself. *)\n"

let base =
  [
    "lib/a.mli" => a_mli;
    "lib/a.ml" => "let f x = x\n";
    "test/t.ml" => t_ml;
    "test/fixtures/x.cwl" => "class: CommandLineTool\n";
    "test/mutants/f.json" => mutant ();
    "gates.json" => config;
  ]

let why = "\n\nThe reason."
let test = [ "test/t.ml" => t_ml ^ "let () = assert (A.f 0 = 0)\n" ]
let new_mutant = [ "test/mutants/g.json" => mutant ~old:"+ 0" ~fresh:"+ 1" ~why:"f adds nothing" () ]
let fix = test @ new_mutant (* a feat or fix whose changed line of lib/a.ml holds "+ 0" *)
let long = "let f x =\n  let y = x + 0 in\n  let z = y in\n  z\n"
(* 4 code lines, over the limit of 3 *)

let shrink = [ "lib/a.ml" => "let f = Fun.id\n" ]
let plus_zero = "lib/a.ml" => "let f x = x + 0\n"
let g_mli doc = "lib/a.mli" => a_mli ^ doc
let g_ml body = "lib/a.ml" => "let f x = x\n" ^ body

(* name, files changed (None deletes), commit message, a phrase the problems
   must contain (None: no problem at all) *)
let cases =
  [
    ("refactor that grows", [ "lib/a.ml" => "let f x =\n  x\n" ], "refactor: Grow f", Some "must not grow");
    ( "code outside lib counts",
      [ "bin/main.ml" => "(** Main. *)\n\nlet () = ()\n" ],
      "refactor: Add main",
      Some "must not grow" );
    ("feat within budget", g_ml "let g x = x + 0\n" :: fix, "feat: Add g", None);
    ("tests over budget", [ "test/t.ml" => t_ml ^ rep 41 "let () = ()\n" ], "test: Add many", Some "tests +41 over +40");
    ( "fixture lines are tests",
      [ "test/fixtures/x.cwl" => rep 42 "a\n" ],
      "test: Add a fixture",
      Some "tests +41 over +40" );
    ("binary fixtures count nothing", [ "test/fixtures/x.bin" => rep 50 "\000\n" ], "test: Add a blob", None);
    ("docs changing a comment", [ "lib/a.ml" => "(* f is the identity *)\nlet f x = x\n" ], "docs: Explain f", None);
    ("docs changing code", [ plus_zero ], "docs: Explain f", Some "changes code in lib/a.ml");
    ( "docs changing a string",
      [ "test/t.ml" => sub "identity" "id" t_ml ],
      "docs: Reword",
      Some "changes code in test/t.ml" );
    ("test touching lib", plus_zero :: test, "test: Sneak", Some "changes code in lib/a.ml");
    ("fix without a test", plus_zero :: new_mutant, "fix: Change f", Some "needs a test");
    ( "fix with a comment as its test",
      plus_zero :: ("test/t.ml" => t_ml ^ "(* note *)\n") :: new_mutant,
      "fix: Change f",
      Some "needs a test" );
    ("fix without a mutant", plus_zero :: test, "fix: Change f", Some "adds a mutant");
    ( "fix whose mutant is on a line it did not change",
      g_ml "let h = 2 + 0\n" :: ("test/mutants/g.json" => mutant ~fresh:"x = 5" ()) :: test,
      "fix: Add h",
      Some "adds a mutant" );
    ( "fix whose mutant is in a test",
      plus_zero :: ("test/mutants/g.json" => mutant ~file:"test/t.ml" ~old:"A.f 0 = 0" ~fresh:"A.f 0 = 1" ()) :: test,
      "fix: Change f",
      Some "adds a mutant" );
    ( "refactor changing an expectation",
      [ "test/t.ml" => sub "identity" "id" t_ml ],
      "refactor: Rename",
      Some "changes test expectations" );
    ( "refactor changing a number",
      [ "test/t.ml" => sub "A.f 1 = 1" "A.f 1 = 2" t_ml ],
      "refactor: Tidy",
      Some "changes test expectations" );
    ("refactor renaming in a test", [ "test/t.ml" => sub "let quote" "let q" t_ml ], "refactor: Rename", None);
    ( "refactor touching an oracle case",
      [ "test/oracle/c.yml" => "x: 1\n" ],
      "refactor: Move",
      Some "changes test expectations" );
    ( "lib/runtime.ml may do I/O",
      [
        "lib/runtime.mli"
        => "(** Runtime. *)\n\nval env : unit -> string\n(** [env ()] is the variable X, read now. *)\n";
        "lib/runtime.ml" => "let env () = Sys.getenv \"X\"\n";
        plus_zero;
      ]
      @ fix,
      "feat: Read X",
      None );
    ( "code hidden in an extension string",
      [ "lib/a.ml" => "let f x = x\n[@@@a {%x|(*|}]\nlet g x = x\n[@@@a {%x|*)|}]\n" ],
      "refactor: Hide g",
      Some "must not grow" );
    ("stray top-level file", [ "notes.md" => "scratch\n" ], "docs: Add notes", Some "at the top level");
    ( "new copy in a subfolder",
      [ "lib/b/c.ml" => "(** C. *)\n\nlet f x =\n  let y = x in\n  y\n\nlet g z =\n  let w = z in\n  w\n" ],
      "refactor: Add c",
      Some "copied lines in lib/ rise" );
    ( "new copy in test",
      [ "test/t.ml" => t_ml ^ "let a () =\n  let b = 1 in\n  b\n\nlet c () =\n  let d = 2 in\n  d\n" ],
      "test: Add two",
      Some "copied lines in test/ rise" );
    ( "one repeated line is a table",
      [ "test/t.ml" => t_ml ^ "let rows =\n  [\n" ^ rep 8 "    \"a\";\n" ^ "  ]\n" ],
      "test: Add rows",
      None );
    ("mutant swapped", [ "test/mutants/f.json" => mutant ~fresh:"x = 2" () ], "test: Swap", Some "is gone");
    ( "mutant moved with its line",
      [ "lib/a.ml" => "let f y = y\n"; "test/mutants/f.json" => mutant ~old:"y = y" ~fresh:"y = 0" () ],
      "refactor: Rename x",
      None );
    ("mutant deleted", [ ("test/mutants/f.json", None) ], "test: Drop it", Some "mutants 1→0");
  ]

(* Messages, on a commit that shrinks the code: the message, and the phrase. *)
let messages =
  [
    ("refactor: Shrink f", None);
    ("Shrink f", Some "not `type(scope): Summary`");
    ("perf: Shrink f", Some "not `type(scope): Summary`");
    ("revert: Shrink f", Some "not `type(scope): Summary`");
    ("refactor(command_line_tool.v1): Shrink f", None);
    ("refactor: Shrink f" ^ why ^ "\n\nMade-with: An editor", Some "found `Made-with:`");
    ("refactor: Shrink f" ^ why ^ "\n\nGenerated with an assistant.", Some "found `Generated with`");
    ("refactor: Shrink f as discussed with Grok", Some "names a tool");
    ("refactor: Move the YAML cursor", None);
    ("refactor: Shrink f\n", Some "no body");
  ]

(* A fix with a test and a mutant, whose lib/a.ml is the given text. *)
let fixes =
  [
    ("let f x = x + 0\n", None);
    ("let f x = if x < 0 then failwith \"neg\" else x + 0\n", Some "calls `failwith`");
    ("(* cannot raise: [x] is not empty *)\nlet f x = List.hd [ x + 0 ]\n", None);
    ("(* cannot raise *)\nlet f x = List.hd [ x + 0 ]\n", Some "calls `List.hd`");
    ("let f x = ignore \"cannot raise: it is fine\"; List.hd [ x + 0 ]\n", Some "calls `List.hd`");
    ("(* failwith *)\nlet f x = ignore \"failwith\"; x + 0\n", None);
    ("let f x = x + 0\nlet js = {js|\nprocess.exit(1); var ref = 0;\n|js}\n", None);
    ("type t = { exit : int; ref : int }\nlet f x = x.exit + 0\n", None);
    ("let f x = ignore (Sys.getenv \"X\"); x + 0\n", Some "uses `Sys`");
    ("module H = Hashtbl\nlet f x = x + 0\n", Some "uses `Hashtbl`");
    ("let f x = ignore (open_in \"x\"); x + 0\n", Some "uses `open_in`");
    ("type c = { mutable n : int }\nlet f x = x + 0\n", Some "uses `mutable`");
    ("let f x = let r = ref (x + 0) in !r\n", Some "uses `ref`");
    ("let f x = x + 0 [@@ocamlformat \"disable\"]\n", Some "switches ocamlformat off");
    ("let f x = x + 0 in\n", Some "lib/a.ml:2 does not parse");
    ("let f x =\n  let y = x + 0 in\n  y\n", None);
    (long, Some "`f` (lib/a.ml, 4 code lines)");
    ( "module M = struct\n" ^ String.concat "" (List.map (fun l -> "  " ^ l ^ "\n") (Repo.lines long)) ^ "end\n",
      Some "`f` (lib/a.ml, 4 code lines)" );
    (sub "let f x =" "let () =" long, Some "`()` (lib/a.ml, 4 code lines)");
    (long ^ "let f x = x\n", Some "`f` (lib/a.ml, 4 code lines)");
    ( "let f x =\n  let y = x in\n  y\n\nlet g z =\n  let w = z in\n  w\n\nlet h = 1 + 0\n",
      Some "copied lines in lib/ rise" );
  ]

(* A feat that adds g to lib/a.mli, with this text after f's val. *)
let vals =
  [
    ("\nval g : int\n", Some "no doc comment of 4 or more words after lib/a.mli: g");
    ("\nval g : int\n(** G. *)\n", Some "no doc comment of 4 or more words");
    ("\n(** [g] is one more than nothing at all. *)\nval g : int\n", Some "after lib/a.mli: g");
    ("\nval g : int\n(** [g] is one more than the\n    type of nothing, in words. *)\n", None);
    ("\nval g : path:string -> int\n(** [g ~path] is one, whatever the path. *)\n", Some "a label is only");
    ("\nval g : a:string -> b:string -> int\n(** [g ~a ~b] is one, whatever they are. *)\n", None);
    ("\nval g : ?n:int -> quiet:bool -> int\n(** [g ?n ~quiet] is one, louder or not. *)\n", None);
  ]

let all_cases =
  cases
  @ List.map (fun (m, e) -> (m, shrink, m, e)) messages
  @ List.map (fun (src, e) -> (String.escaped src, ("lib/a.ml" => src) :: fix, "fix: Change f", e)) fixes
  @ List.map (fun (doc, e) -> (String.escaped doc, g_mli doc :: g_ml "let g = 1 + 0\n" :: fix, "feat: Add g", e)) vals

let git args = ignore (Repo.git args)
let gates = Filename.concat (Sys.getcwd ()) "../gates.exe"
let fails r = r.Repo.failed
let has phrase lines = List.exists (Repo.contains phrase) lines

let commit files message =
  List.iter (fun (path, text) -> Repo.write path text) files;
  git [ "add"; "-A" ];
  git [ "commit"; "-q"; "--allow-empty"; "-m"; (if String.contains message '\n' then message else message ^ why) ];
  String.trim (Repo.git [ "rev-parse"; "HEAD" ])

let problems files message = Commit.problems (Commit.make (Config.of_string config) (commit files message))

(** [f base] in a new repository whose first commit [base] holds [base]. *)
let in_repo f () =
  let home = Sys.getcwd () in
  Repo.with_temp_dir (fun dir ->
      Sys.chdir dir;
      Fun.protect
        ~finally:(fun () -> Sys.chdir home)
        (fun () ->
          git [ "init"; "-q"; "-b"; "main" ];
          f (commit base "chore: Start")))

let case (name, files, message, expect) =
  Alcotest.test_case name `Slow
    (in_repo (fun _ ->
         let found = problems files message in
         match expect with
         | None -> Alcotest.(check (list string)) "no problem" [] found
         | Some phrase ->
             if not (has phrase found) then Alcotest.failf "no %S in [%s]" phrase (String.concat "; " found)))

let check_true what b = Alcotest.(check bool) what true b

let long_renamed _ =
  ignore (commit [ "lib/a.ml" => long ] "feat: Spell f out");
  Alcotest.(check (list string))
    "a rename keeps the sum" []
    (problems [ "lib/a.ml" => sub "let f" "let g" long ] "refactor: Rename f");
  check_true "rise" (has "rise 1→2" (problems [ "lib/a.ml" => sub "z\n" "z\n  |> Fun.id\n" long ] "refactor: Grow f"))

let total _ =
  let start = commit [ "lib/a.ml" => "let f x = x\nlet g x = x\nlet h x = x\n" ] "feat: Add g and h" in
  ignore (commit [ "lib/a.ml" => "let f x = x\n" ] "refactor: Drop g and h");
  ignore (commit [ "test/t.ml" => t_ml ^ "let () = ()\n" ] "test: Add one");
  check_true "total row" (has "| all 2 | | -2 | +1 |" (Commit.base start "HEAD").rows)

let symbolic_link _ =
  Unix.symlink "../test/fixtures/x.cwl" "lib/b.ml";
  check_true "link" (has "symbolic link" (problems [] "refactor: Link b"))

let merge base =
  git [ "checkout"; "-q"; "-b"; "side" ];
  ignore (commit shrink "refactor: Shrink f");
  git [ "checkout"; "-q"; "-" ];
  ignore (commit [ "test/t.ml" => t_ml ^ "let () = ()\n" ] "test: Add one");
  git [ "merge"; "-q"; "--no-ff"; "side"; "-m"; "Merge side" ];
  Alcotest.(check int) "the merge fails" 1 (fails (Commit.base base "HEAD"));
  Alcotest.check_raises "a bad base stops the gate" (Repo.Fail "") (fun () ->
      try ignore (Commit.base "no-such-ref" "HEAD") with Repo.Fail _ -> raise (Repo.Fail ""))

let exit_code ?(cwd = ".") args = match Repo.run ~cwd gates args with code, _, _ -> code

let worktree base =
  List.iter (fun (p, t) -> Repo.write p t) [ "lib/a.ml" => "let f x =\n  x\n"; "notes.md" => "untracked scratch\n" ];
  Alcotest.(check int) "grown" 1 (fails (Commit.worktree ("refactor: Grow f" ^ why)));
  Alcotest.(check string) "HEAD stays" base (String.trim (Repo.git [ "rev-parse"; "HEAD" ]));
  Alcotest.(check (option int))
    "from a subfolder" (Some 1)
    (exit_code ~cwd:"lib" [ "--worktree"; "-m"; "refactor: Grow f" ^ why ]);
  Alcotest.(check (option int)) "the message decides the budget" (Some 2) (exit_code [ "--worktree" ]);
  Repo.write "lib/a.ml" (Some "let f x = x + 1\n");
  Alcotest.(check int) "a feat needs a test and a mutant" 1 (fails (Commit.worktree ("feat: Shift f" ^ why)))

(** Put a [dune] that runs [script] first on the PATH while [f] runs. *)
let fake_dune script f =
  let path = Sys.getenv "PATH" in
  Repo.write "bin-fake/dune" (Some ("#!/bin/sh\n" ^ script));
  Unix.chmod "bin-fake/dune" 0o755;
  Repo.write ".git/info/exclude" (Some "bin-fake\n");
  Unix.putenv "PATH" (Filename.concat (Sys.getcwd ()) "bin-fake" ^ ":" ^ path);
  Fun.protect ~finally:(fun () -> Unix.putenv "PATH" path) f

(* The fake build lists test/names.txt as the test cases; the fake tests pass
   when lib/a.ml holds the text of test/expect.txt. *)
let each_dune =
  {|case "$1" in
build) grep -q BROKEN lib/a.ml && exit 1
  mkdir -p _build/default/test
  printf '#!/bin/sh\nsed "s/^/t 0 /;s/$/./" test/names.txt\n' > _build/default/test/test_cwl.exe
  chmod +x _build/default/test/test_cwl.exe; exit 0 ;;
runtest) [ -f test/expect.txt ] && ! grep -qF "$(cat test/expect.txt)" lib/a.ml && exit 1; exit 0 ;;
esac
exit 0
|}

let each _ =
  fake_dune each_dune (fun () ->
      let start = commit [ "test/names.txt" => "one\ntwo\n" ] "test: Name the cases" in
      ignore (commit [ "lib/a.ml" => "let f x = x (* BROKEN *)\n" ] "refactor: Break");
      ignore (commit [ "lib/a.ml" => "let f x = x\n"; "test/names.txt" => "one\n" ] "test: Drop a case");
      ignore (commit ([ plus_zero; "test/expect.txt" => "x + 0" ] @ fix) "fix: Add zero");
      ignore
        (commit
           (("lib/a.ml" => "let f x = x + 0 + 0\n")
           :: ("test/mutants/h.json" => mutant ~old:"0 + 0" ~fresh:"0" ())
           :: test)
           "fix: Add more");
      let r = Build.each start "HEAD" in
      Alcotest.(check int) "the broken build, the dropped case, the fix its tests don't notice" 3 (fails r);
      check_true "gone" (has "gone: t: two" r.rows);
      check_true "bites" (has "pass with the parent's code" r.rows);
      Alcotest.(check (option string))
        "the checkout never moved" (Some "let f x = x + 0 + 0\n") (Repo.read Disk "lib/a.ml");
      Alcotest.(check int) "no worktree left" 1 (List.length (Repo.lines (Repo.git [ "worktree"; "list" ]))))

let mutants _ =
  fake_dune {|[ "$1" = runtest ] && grep -q "x = 0" lib/a.ml && exit 1; exit 0|} (fun () ->
      List.iter
        (fun (p, t) -> Repo.write p t)
        [
          "test/mutants/g.json" => mutant ~fresh:"x = 5" ~why:"survivor" ();
          "test/mutants/h.json" => mutant ~old:"nowhere" ~why:"stale" ();
          "test/mutants/i.json" => mutant ~file:"test/t.ml" ~why:"a test" ();
        ];
      let r = Build.mutants () in
      Alcotest.(check int) "three bad mutants" 3 (fails r);
      List.iter (fun p -> check_true p (has p r.rows)) [ "survives"; "found 0 times"; "not a file in lib/ or bin/" ];
      Alcotest.(check (option string)) "a scratch copy only" (Some "let f x = x\n") (Repo.read Disk "lib/a.ml"))

let mutants_need_passing_tests _ =
  fake_dune {|[ "$1" = runtest ] && exit 1; exit 0|} (fun () ->
      let r = Build.mutants () in
      Alcotest.(check int) "one failure" 1 (fails r);
      check_true "says why" (has "with no mutant the code is fail" r.rows))

let code () =
  let lines src = List.map fst (Source.code_lines src) in
  let src = "let a = \"(* not a comment\" (* a (* nested *) \"*)\" *) let b = '\"'\nlet c = {id|*)|id}\n(* only *)\n" in
  Alcotest.(check (list int)) "comments and strings" [ 1; 2 ] (lines src);
  Alcotest.(check (list (pair int string)))
    "shape" (Source.code_lines "let y = g \"t\" 3")
    (Source.code_lines "let x = f \"s\" 12");
  Alcotest.(check (list int)) "a multi-line string" [ 1; 2; 3; 4 ] (lines "let s = {|\na\nb\n|}\n");
  Alcotest.(check (list int)) "an extension string" [ 1; 2; 3 ] (lines "[@@@h {%a|(*|}]\nlet x = 1\n[@@@h {%a|*)|}]\n");
  check_true "a table" (Commit.tabular (List.concat (List.init 5 (Fun.const [ "a"; "b" ]))));
  check_true "not a table" (not (Commit.tabular (List.init 10 string_of_int)))

(** The PR template, found from the test's directory up. *)
let template =
  let rec up dir =
    let path = Filename.concat dir ".github/pull_request_template.md" in
    if Sys.file_exists path then Repo.slurp path
    else if Filename.dirname dir = dir then "no template"
    else up (Filename.dirname dir)
  in
  up (Sys.getcwd ())

let json s =
  let escape = function '"' -> {|\"|} | '\\' -> {|\\|} | '\n' -> {|\n|} | c -> String.make 1 c in
  "\"" ^ String.concat "" (List.map escape (List.of_seq (String.to_seq s))) ^ "\""

let pr base =
  git [ "update-ref"; "refs/remotes/origin/main"; base ];
  let ocaml = commit shrink "refactor: Shrink f" in
  let body = "**What**\n- x\n\n**Why** y\n\n**How it works**\nz\n\n**OCaml worth noticing**\n- z\n"
  and short = "**What**\n- x\n\n**Why** y\n" in
  let event ?(title = "fix: Do x") ?(body = body) ?(base = "main") ?(head = ("fix/do-x.v2", ocaml)) () =
    Printf.sprintf
      {|{"pull_request": {"number": 1, "title": %s, "body": %s, "base": {"ref": %s}, "head": {"ref": %s, "sha": %s}},
      "repository": {"default_branch": "main"}}|}
      (json title) (json body) (json base)
      (json (fst head))
      (json (snd head))
  in
  List.iter
    (fun (expect, json) ->
      Repo.write "event.json" (Some json);
      Alcotest.(check int) json expect (fails (Pr.check "event.json")))
    [
      (0, event ());
      (1, event ~title:"Do x" ());
      (1, event ~body:(body ^ "\nTest plan: 112 tests pass") ());
      (2, event ~body:short ());
      (2, event ~body:(short ^ "There's no OCaml in this PR.") ());
      (0, event ~body:short ~head:("fix/do-x", base) ());
      (1, event ~body:(sub "z\n\n" "<!-- hint -->\n\n" body) ());
      (5, event ~body:template ());
      (1, event ~base:"fix/other" ());
      (1, event ~head:("my-branch", ocaml) ());
      (1, event ~body:(body ^ "\n\xF0\x9F\xA4\x96 Generated") ());
    ]

let () =
  List.iter
    (fun (k, v) -> Unix.putenv k v)
    [
      ("GIT_AUTHOR_NAME", "t");
      ("GIT_AUTHOR_EMAIL", "t@t");
      ("GIT_COMMITTER_NAME", "t");
      ("GIT_COMMITTER_EMAIL", "t@t");
      ("GIT_CONFIG_GLOBAL", "/dev/null");
      ("GIT_CONFIG_NOSYSTEM", "1");
      ("GIT_CONFIG_COUNT", "1");
      ("GIT_CONFIG_KEY_0", "maintenance.auto");
      ("GIT_CONFIG_VALUE_0", "false");
    ];
  (* The cases that run git are `Slow: the quick tests that --each and
     --mutants run are ccr's own. *)
  let repo name f = Alcotest.test_case name `Slow (in_repo f) in
  Alcotest.run "gates"
    [
      ("commit", List.map case all_cases);
      ( "modes",
        [
          repo "long function renamed" long_renamed;
          repo "total" total;
          repo "symbolic link" symbolic_link;
          repo "merge commit" merge;
          repo "worktree" worktree;
          repo "each" each;
          repo "mutants" mutants;
          repo "mutants need passing tests" mutants_need_passing_tests;
          Alcotest.test_case "code" `Quick code;
          repo "pr" pr;
        ] );
    ]
