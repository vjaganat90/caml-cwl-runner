(** The hard limits of AGENTS.md ("Size and shape"), checked commit by commit.

    {v
    gates --base REF [--head REV]   every commit in REF..REV (default HEAD),
                                    against its parent
    gates --worktree -m MESSAGE     the uncommitted changes, as one commit with
                                    that message, before you commit them
    gates --each --base REF [--head REV]
                                    build every commit of the range: it is
                                    formatted, its message names each test case
                                    it removes, and a feat or fix has quick
                                    tests that fail on its parent
    gates --mutants                 every mutant in test/mutants/ builds and
                                    makes the quick tests fail
    gates --dups                    the copied blocks in lib/ and test/, as the
                                    files are on disk now
    gates --pr EVENT_JSON           a PR's title, body and branches
    v}

    Every failure says how to fix it, and the gate exits 1. It runs from any directory of the repository. --each and
    --mutants work in a scratch copy, so the files you work on never change. CI builds the gate and reads gates.json as
    they are on the base branch, so a PR that edits them is measured by the old ones, and .github/CODEOWNERS makes the
    owner read any change to them or to the workflows that run them.

    A mutant is a file test/mutants/<behavior>.json holding [{"file", "old", "new", "why"}]: [old] occurs exactly once
    in [file]; the gate swaps in [new], which must build, and the quick tests (the Alcotest cases marked [`Quick]) must
    then fail. A report row reads: commit, type, code lines added / budget, test lines added / budget, copied lines in
    lib/ and test/ (before -> after), and the problems found. *)

open Gate

exception Terminated

let usage =
  "gates (--base REF [--head REV] [--each] | --worktree -m MESSAGE | --mutants | --dups | --pr EVENT_JSON)...\n\
   The hard limits of AGENTS.md (\"Size and shape\"); scripts/gates/gates.ml says what each mode checks."

(** What the command line asks for: whether to list the copied blocks, and the checks to run, in order. A wrong command
    line exits 2. *)
let command () =
  (* Arg stores what it parses through refs. *)
  let base = ref None and head = ref "HEAD" and worktree = ref false and message = ref None in
  let each = ref false and mutants = ref false and dups = ref false and pr = ref None in
  let some r = Arg.String (fun s -> r := Some s) in
  Arg.parse
    [
      ("--base", some base, "REF  check every commit after REF");
      ("--head", Arg.Set_string head, "REV  the last commit of the range (default HEAD)");
      ("--worktree", Arg.Set worktree, " check the uncommitted changes as one commit");
      ("-m", some message, "MESSAGE  the full message the working tree would be committed with");
      ("--each", Arg.Set each, " build every commit of the range");
      ("--mutants", Arg.Set mutants, " apply every mutant to a scratch copy");
      ("--dups", Arg.Set dups, " list the copied blocks");
      ("--pr", some pr, "EVENT_JSON  check the PR of a GitHub event");
    ]
    (fun a -> raise (Arg.Bad ("unexpected argument " ^ a)))
    usage;
  let stop e =
    prerr_endline e;
    exit 2
  in
  if not (!base <> None || !worktree || !mutants || !dups || !pr <> None) then stop usage;
  if !worktree && !message = None then stop "gates: --worktree needs -m MESSAGE: the commit's type decides its budget";
  if !each && !base = None then stop "gates: --each needs --base REF";
  let absolute p = if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p in
  ( !dups,
    List.concat
      [
        (match !pr with Some p -> [ (fun () -> Pr.check (absolute p)) ] | None -> []);
        (match !message with Some m when !worktree -> [ (fun () -> Commit.worktree m) ] | _ -> []);
        (match !base with
        | Some b when !each -> [ (fun () -> Build.each b !head) ]
        | Some b -> [ (fun () -> Commit.base b !head) ]
        | None -> []);
        (if !mutants then [ Build.mutants ] else []);
      ] )

let () =
  let dups, checks = command () in
  let report failed check =
    let r = check () in
    Repo.print r;
    failed + r.Repo.failed
  in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> raise Terminated));
  try
    Sys.chdir (String.trim (Repo.git [ "rev-parse"; "--show-toplevel" ]));
    (if dups then
       let window = (Config.at (At "HEAD")).window in
       let spans = List.concat_map (fun f -> Commit.spans (Commit.copied Disk f window)) [ "lib/"; "test/" ] in
       print_endline (if spans = [] then "no copied blocks" else String.concat "\n" spans));
    exit (if List.fold_left report 0 checks > 0 then 1 else 0)
  with
  | Repo.Fail m ->
      prerr_endline m;
      exit 1
  | Terminated -> exit 143
