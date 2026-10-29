(** The checks that build: [--each] builds every commit of a range in a scratch worktree, and [--mutants] applies each
    mutant to a scratch copy of the working tree. The files you work on never change. *)

open Repo

(** The exit code of [dune args] run in [tree], or [None] after 10 minutes. *)
let dune ?env tree args =
  match run ~cwd:tree ?env ~timeout:600. "dune" args with
  | Some 127, _, _ -> fail "dune not found: run this under `opam exec --`"
  | code, _, _ -> code

(** How the code in [tree] fares: "broken" when it does not build, else whether its quick tests (the Alcotest cases
    marked [`Quick]) "pass" or "fail", or "timeout". *)
let tests tree =
  let quick = env_with [ ("ALCOTEST_QUICK_TESTS", "1"); ("ALCOTEST_BAIL", "1") ] in
  match dune tree [ "build" ] with
  | Some 0 -> (
      match dune ~env:quick tree [ "runtest"; "--force" ] with Some 0 -> "pass" | Some _ -> "fail" | None -> "timeout")
  | Some _ -> "broken"
  | None -> "timeout"

(** The test cases of the build in [tree], as "suite: name". *)
let test_names tree =
  let exe = Filename.concat tree "_build/default/test/test_cwl.exe" in
  let case = Re.Perl.compile_pat ~opts:[ `Multiline ] {|^(\S+) +\d+ +(.*)\.$|} in
  let _, listed, _ = if Sys.file_exists exe then run ~cwd:tree exe [ "list" ] else (None, "", "") in
  List.map (fun g -> Re.Group.get g 1 ^ ": " ^ Re.Group.get g 2) (Re.all case listed)

(** Check out [sha] in [tree], whose last commit had the test cases [names]: it builds and is formatted, every case it
    removes is named in its message, and a feat or fix that changes code has quick tests that fail with its parent's
    code. *)
let step config tree names sha =
  let c = Commit.make config sha in
  ignore (git [ "-C"; tree; "checkout"; "-q"; "-f"; "--detach"; sha ]);
  ignore (git [ "-C"; tree; "clean"; "-q"; "-f"; "-d" ]);
  let built = dune tree [ "build" ] = Some 0 && dune tree [ "build"; "@fmt" ] = Some 0 in
  let now = if built then test_names tree else names in
  let lost = List.sort compare (List.filter (fun n -> not (List.mem n now || contains n c.message)) names) in
  let revert (f : Commit.file) =
    if Commit.measured f.path = Some Code then write (Filename.concat tree f.path) f.before
  in
  let bites () =
    List.iter revert c.files;
    if tests tree <> "pass" then "ok"
    else "the quick tests pass with the parent's code: add one that fails without this commit"
  in
  ( now,
    [
      (if built then "ok" else "does not build or is not formatted: fix it in this commit");
      (if lost = [] then "ok"
       else "gone: " ^ String.concat ", " lost ^ "; name each in the message, with what covers it now");
      (if List.mem c.kind [ Some "feat"; Some "fix" ] && Commit.changes_code c then bites () else "-");
    ] )

(** Every commit of [base..head], built in a scratch worktree. *)
let each base head =
  let config = Config.at (At base) and shas = Commit.commits base head in
  let row sha cells = Printf.sprintf "| %s | %s |" (String.sub sha 0 7) (String.concat " | " cells) in
  let check tree =
    let names = if dune tree [ "build" ] = Some 0 then test_names tree else [] in
    let _, cells = List.fold_left_map (step config tree) names shas in
    let failed = List.length (List.filter (List.exists (fun cell -> cell <> "ok" && cell <> "-")) cells) in
    let header =
      [ "| commit | builds and is formatted | test cases kept | tests fail without its code |"; "|---|---|---|---|" ]
    in
    { title = "Every commit"; rows = header @ List.map2 row shas cells; failed }
  in
  with_temp_dir (fun tmp ->
      let tree = Filename.concat tmp "tree" in
      ignore (git [ "worktree"; "add"; "-q"; "--detach"; tree; base ]);
      Fun.protect ~finally:(fun () -> ignore (git [ "worktree"; "remove"; "--force"; tree ])) (fun () -> check tree))

(** What the quick tests make of the mutant [m], applied alone in [tree]. *)
let mutant tree code (m : Config.mutant) =
  let src = text Disk m.file and path = Filename.concat tree m.file in
  let outcome () =
    write path (Some (Re.replace_string (Re.compile (Re.str m.old)) ~by:m.fresh src));
    Fun.protect ~finally:(fun () -> write path (Some src)) (fun () -> tests tree)
  in
  match find m.old src with
  | _ when not (List.mem m.file code) ->
      m.file ^ " is not a file in lib/ or bin/: point `file` at the code that holds the behavior"
  | [ _ ] -> (
      match outcome () with
      | "fail" -> "ok"
      | "broken" -> "the mutated code does not build: choose a mutation that compiles"
      | "pass" -> "survives: no quick test notices this behavior break; add or fix the test"
      | _ -> "the build or the tests time out")
  | found ->
      Printf.sprintf "its `old` is found %d times in %s: anchor it on one line (ocamlformat may have re-wrapped it)"
        (List.length found) m.file

(** Copy the working tree, check that it passes the quick tests, then apply each mutant alone: it must build and fail
    them. *)
let mutants () =
  let header = [ "| mutant | result |"; "|---|---|" ] in
  let row ((m : Config.mutant), result) = Printf.sprintf "| %s: %s | %s |" m.file m.why result in
  let check tmp =
    List.iter
      (fun p -> if not (under [ "vendor/" ] p) then write (Filename.concat tmp p) (read Disk p))
      (paths Disk ".");
    match tests tmp with
    | "pass" ->
        let code = paths Disk "lib/" @ paths Disk "bin/" in
        let results = List.map (fun m -> (m, mutant tmp code m)) (Config.mutants Disk) in
        {
          title = "Mutants";
          rows = header @ List.map row results;
          failed = List.length (List.filter (fun (_, r) -> r <> "ok") results);
        }
    | start ->
        {
          title = "Mutants";
          rows = header @ [ Printf.sprintf "| (none) | with no mutant the code is %s: fix that first |" start ];
          failed = 1;
        }
  in
  with_temp_dir check
