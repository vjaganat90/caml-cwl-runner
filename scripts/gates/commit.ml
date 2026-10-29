(** One commit measured against its parent, the checks of AGENTS.md ("Size and shape") on it, and the [--base] and
    [--worktree] reports. Each check returns the problems it finds, each with its fix, in the words AGENTS.md uses for
    the rule. *)

open Repo
open Config

let sprintf = Printf.sprintf
let types = "feat|fix|refactor|test|docs|ci|chore"
let subject = Re.Perl.compile_pat ("^(" ^ types ^ {|)(\([a-z0-9_.-]+\))?!?: \S|})

(* A trailer such as Co-authored-by: or Made-with:, a "Generated with" line, a
   robot, or a tool or a conversation named in a commit message or a PR. *)
let leak =
  Re.Perl.compile_pat ~opts:[ `Caseless; `Multiline ]
    (String.concat "|"
       [
         {|^[A-Za-z0-9_-]+-(by|with):|};
         {|^generated (with|by)\b|};
         "\xF0\x9F\xA4\x96";
         {|\b(grok|claude|copilot|chatgpt|codex|gemini|windsurf|aider|as discussed|per (the )?review|future sessions?)\b|};
       ])

let ocaml p = List.exists (Filename.check_suffix p) [ ".ml"; ".mli"; ".mll"; ".mly" ]
let ml p = Filename.check_suffix p ".ml"
let mli p = Filename.check_suffix p ".mli"

type measure = Code | Tests

(** Whether [p] counts as code, as tests, or not at all. The gate's own code under scripts/ is not counted. *)
let measured p =
  if under [ "test/fixtures/" ] p then Some Tests
  else if not (ocaml p) then None
  else if under [ "test/" ] p then Some Tests
  else if under [ "lib/"; "bin/" ] p then Some Code
  else None

(** The lines of [src] that count: the code lines of OCaml, every non-blank line of other text, nothing of a binary
    file. *)
let counted p = function
  | None -> 0
  | Some src when Source.ocaml p -> List.length (Source.code_lines src)
  | Some src -> if String.contains src '\000' then 0 else List.length (nonblank src)

(** Whether the shapes of [run] repeat with a period of at most half its length, like the rows of a table. *)
let tabular run =
  let a = Array.of_list run in
  let period p = Seq.for_all (fun i -> a.(i) = a.(i + p)) (Seq.init (Array.length a - p) Fun.id) in
  Seq.exists period (Seq.init (Array.length a / 2) succ)

module Runs = Map.Make (String)

(** The (path, line) of every code line of the .ml files under [folder] at [rev] that sits in a run of [window] code
    lines whose shape appears again under that folder. The rows of a table are not a copy. *)
let copied rev folder window =
  let files =
    List.map (fun p -> (p, Array.of_list (Source.code_lines (text rev p)))) (List.filter ml (paths rev folder))
  in
  let run (p, lines) k = (List.init window (fun i -> snd lines.(k + i)), (p, k)) in
  let runs (p, lines) = List.init (max 0 (Array.length lines - window + 1)) (run (p, lines)) in
  let add m (run, at) = if tabular run then m else Runs.add_to_list (String.concat "\n" run) at m in
  let copies =
    Runs.filter (fun _ ats -> List.length ats > 1) (List.fold_left add Runs.empty (List.concat_map runs files))
  in
  let lines (p, k) = List.init window (fun i -> (p, fst (List.assoc p files).(k + i))) in
  List.sort_uniq compare (List.concat_map lines (List.concat_map snd (Runs.bindings copies)))

(** [lines] as "path:first-last" spans; a gap of more than 12 lines ends one. *)
let spans lines =
  let join acc (p, n) =
    match acc with (q, a, b) :: rest when q = p && n <= b + 12 -> (q, a, n) :: rest | _ -> (p, n, n) :: acc
  in
  List.rev_map (fun (p, a, b) -> sprintf "%s:%d-%d" p a b) (List.fold_left join [] lines)

(** (path, name, code lines) of the definitions in lib/ and bin/ at [rev] that are longer than [limit]. *)
let long rev limit =
  let over p (name, n) = if n > limit then Some (p, name, n) else None in
  List.concat_map
    (fun p -> List.filter_map (over p) (Source.definitions (text rev p)))
    (List.filter ml (paths rev "lib/" @ paths rev "bin/"))

type tree = {
  rev : string;
  mutants : mutant list Lazy.t;
  vals : (string * Source.value) list Lazy.t;  (** The vals of lib/*.mli. *)
  long : (string * string * int) list Lazy.t;
  copies : (string * (string * int) list Lazy.t) list;  (** lib/ and test/ *)
}
(** What the checks read of the whole tree of a commit, each part read once and only when a check needs it. A commit's
    parent is the commit before it in a range, so a range reads each tree once. *)

let tree config rev =
  let at = At rev in
  let vals p = List.map (fun v -> (p, v)) (Source.vals (text at p)) in
  let copies folder = (folder, lazy (copied at folder config.window)) in
  {
    rev;
    mutants = lazy (mutants at);
    vals = lazy (List.concat_map vals (List.filter mli (paths at "lib/")));
    long = lazy (long at config.max_function);
    copies = List.map copies [ "lib/"; "test/" ];
  }

type file = { path : string; before : string option; after : string option; lines : int list Lazy.t }
(** A file the commit changes: its text before and after, and the lines of [after] that differ. *)

type t = {
  sha : string;
  parent : string;
  message : string;
  kind : string option;  (** The type of the subject, if it is conventional. *)
  config : Config.t;
  parent_tree : tree;
  tree : tree;
  files : file list;
}

(** The commit [sha], measured against its parent, whose tree is [before] when [before] is the parent's. *)
let make ?before config sha =
  let parent = String.trim (git [ "rev-parse"; sha ^ "^" ]) in
  let parent_tree = match before with Some t when t.rev = parent -> t | _ -> tree config parent in
  let tree = tree config sha in
  let message = git [ "log"; "-1"; "--format=%B"; sha ] in
  let kind = Option.map (fun g -> Re.Group.get g 1) (Re.exec_opt subject message) in
  let file path =
    { path; before = read (At parent) path; after = read (At sha) path; lines = lazy (touched parent sha path) }
  in
  let files = List.map file (split '\000' (git [ "diff"; "--no-renames"; "--name-only"; "-z"; parent; sha ])) in
  { sha; parent; message; kind; config; parent_tree; tree; files }

(** The code and test lines [c] adds, net. *)
let net c =
  let delta m f = if measured f.path = Some m then counted f.path f.after - counted f.path f.before else 0 in
  let sum m = List.fold_left (fun n f -> n + delta m f) 0 c.files in
  { code = sum Code; tests = sum Tests }

(** For lib/ and test/, when an .ml under it changed: the copied lines before, after, and where they are after. *)
let copies c =
  let count folder (t : tree) = Lazy.force (List.assoc folder t.copies) in
  let measure folder =
    (folder, (List.length (count folder c.parent_tree), List.length (count folder c.tree), count folder c.tree))
  in
  List.map measure
    (List.filter (fun d -> List.exists (fun f -> under [ d ] f.path && ml f.path) c.files) [ "lib/"; "test/" ])

(** Whether [f] changed beyond comments, blank lines and layout. *)
let code_changed f =
  let words = function None -> [] | Some s when Source.ocaml f.path -> Source.words s | Some s -> [ s ] in
  words f.before <> words f.after

(** Whether [f] is OCaml that counts, as code or as tests. *)
let counts f = ocaml f.path && measured f.path <> None

let changes_code c = List.exists (fun f -> measured f.path = Some Code && code_changed f) c.files
let paths_of fs = List.map (fun f -> f.path) fs
let lines_of c p = match List.find_opt (fun f -> f.path = p) c.files with Some f -> Lazy.force f.lines | None -> []

(** Whether the mutant [m] sits on a line that [c] changed. *)
let guards c m =
  let src = text (At c.sha) m.file in
  match find m.old src with
  | [ g ] ->
      let first = 1 + newlines (String.sub src 0 (Re.Group.start g 0)) in
      List.exists (fun n -> n >= first && n <= first + newlines m.old) (lines_of c m.file)
  | _ -> false

let check_message c =
  match (c.kind, Re.exec_opt leak c.message) with
  | None, _ ->
      [
        "the subject is not `type(scope): Summary`: the type is one of feat, fix, refactor, test, docs, ci, chore, and \
         the scope is lowercase letters, digits, `_`, `.` or `-`";
      ]
  | _, Some m ->
      [
        sprintf
          "the message names a tool or carries a trailer (found `%s`): remove it, and turn off your editor's commit \
           attribution"
          (String.trim (Re.Group.get m 0));
      ]
  | _ ->
      when_
        (List.length (nonblank c.message) < 2)
        "the message has no body: add a paragraph that says why the change is needed"

let check_budget c =
  let b = budget c.config c.kind and net = net c in
  let shorter = "a refactor must not grow the code; make it shorter" in
  let split = "make it smaller, or split it into commits that each hold a whole behavior with its test and mutant" in
  when_ (net.code > b.code)
    (sprintf "code %+d over %+d: %s" net.code b.code (if c.kind = Some "refactor" then shorter else split))
  @ when_ (net.tests > b.tests)
      (sprintf "tests %+d over %+d: replace example cases with one property or one table" net.tests b.tests)

(** A docs, ci or chore commit changes no code, and a test commit changes code only under test/. *)
let check_no_code c =
  match c.kind with
  | Some (("docs" | "ci" | "chore" | "test") as k) ->
      let code f = counts f && code_changed f && not (k = "test" && under [ "test/" ] f.path) in
      paths_of (List.filter code c.files)
      >? sprintf "a `%s` commit changes code in %s: move that change to a commit whose type allows it" k
  | _ -> []

(** A feat or fix that changes code changes a test and adds a mutant on a line it changed. *)
let check_tested c =
  match c.kind with
  | Some (("feat" | "fix") as k) when changes_code c ->
      let test f = under [ "test/fixtures/" ] f.path || (under [ "test/" ] f.path && ocaml f.path && code_changed f) in
      let known = List.map key (Lazy.force c.parent_tree.mutants) in
      let fresh m = (not (List.mem (key m) known)) && measured m.file = Some Code && guards c m in
      when_
        (not (List.exists test c.files))
        (sprintf "a `%s` that changes code needs a test that fails without it: a property, a table row or a fixture" k)
      @ when_
          (not (List.exists fresh (Lazy.force c.tree.mutants)))
          (sprintf
             "a `%s` that changes code adds a mutant, test/mutants/<behavior>.json, whose `old` is on a line this \
              commit changed"
             k)
  | _ -> []

(** A refactor changes no test expectation: the literals of test/*.ml, and the files under test/fixtures/ and
    test/oracle/. *)
let check_expectations c =
  let literals = Option.fold ~none:[] ~some:Source.literals in
  let expects f =
    under [ "test/fixtures/"; "test/oracle/" ] f.path
    || (under [ "test/" ] f.path && ocaml f.path && literals f.before <> literals f.after)
  in
  if c.kind <> Some "refactor" then []
  else
    paths_of (List.filter expects c.files)
    >? sprintf "a refactor changes test expectations in %s: a behavior change is a `fix` or `feat` commit of its own"

(** No new top-level path that gates.json doesn't list, and no symbolic link. *)
let check_files c =
  let top p = List.hd (String.split_on_char '/' p) in
  let stray f = f.before = None && not (List.mem (top f.path) c.config.top_level) in
  let rec links = function
    | meta :: path :: rest -> (if contains " 120000 " meta then [ path ] else []) @ links rest
    | _ -> []
  in
  (List.sort_uniq compare (List.map top (paths_of (List.filter stray c.files)))
  >? sprintf
       "adds %s at the top level: stage paths by name, never `git add -A`; a new top-level path is the owner's call \
        (gates.json)")
  @ (links (split '\000' (git [ "diff"; "--raw"; "--no-renames"; "-z"; c.parent; c.sha ]))
    >? sprintf "%s is a symbolic link: commit the file itself")

(** Every changed OCaml file parses: one that does not reads as empty to the other checks. *)
let check_parses c =
  let parses f = if Source.ocaml f.path then Option.bind f.after (Source.problem f.path) else None in
  List.filter_map parses c.files

(** The facts [fact] gives about the vals of lib/*.mli at [c] that its parent did not have, when [c] changes lib/*.mli.
*)
let new_facts c fact =
  let facts (t : tree) = List.concat_map (fun (p, v) -> fact p v) (Lazy.force t.vals) in
  if not (List.exists (fun f -> under [ "lib/" ] f.path && mli f.path) c.files) then []
  else List.sort_uniq compare (List.filter (fun x -> not (List.mem x (facts c.parent_tree))) (facts c.tree))

let check_docs c =
  let undocumented p (v : Source.value) = if List.length (words v.doc) < 4 then [ p ^ ": " ^ v.name ] else [] in
  new_facts c undocumented
  >? sprintf "no doc comment of 4 or more words after %s: say what it returns, how it fails and why it exists"

(** A label on an argument whose type no other argument of the val has, and that is not a bool or an optional. *)
let check_labels c =
  let lonely p (v : Source.value) =
    let alone t = List.length (List.filter (fun (_, u) -> u = t) v.args) = 1 in
    List.filter_map
      (function
        | Asttypes.Labelled l, t when t <> "bool" && alone t -> Some (sprintf "%s: %s ~%s" p v.name l) | _ -> None)
      v.args
  in
  new_facts c lonely >? sprintf "%s: a label is only for arguments that share a type, or for a bool; make it positional"

(** [f file text] for each file of lib/ and bin/ that [c] changes and keeps. *)
let in_code c f =
  let each file = match file.after with Some s when measured file.path = Some Code -> f file s | _ -> [] in
  List.concat_map each c.files

(** Outside lib/runtime.ml and bin/main.ml, a line of code this commit changed does no I/O, keeps no mutable state, and
    raises only where a comment says why it cannot. *)
let check_pure c =
  let pure f = ml f.path && not (List.mem f.path [ "lib/runtime.ml"; "bin/main.ml" ]) in
  let raises =
    "pure code returns an Error.t; if this call truly cannot raise, say why in a `(* cannot raise: ... *)` comment on \
     that line or the one above"
  in
  let at f src uses n =
    let here = List.filter_map (fun (l, u) -> if l = n then Some u else None) uses in
    match List.sort compare here with
    | Source.Impure w :: _ ->
        Some
          (sprintf "%s:%d uses `%s`: I/O and mutable state live in lib/runtime.ml only; take a module argument instead"
             f.path n w)
    | Raises w :: _ when not (Source.says_why src n) -> Some (sprintf "%s:%d calls `%s`: %s" f.path n w raises)
    | _ -> None
  in
  let check f src =
    if pure f then List.filter_map (at f src (Source.uses src)) (List.sort_uniq compare (Lazy.force f.lines)) else []
  in
  in_code c check

(** The changed OCaml files of lib/, bin/ and test/ where [count] of the text grew. *)
let grew c count = paths_of (List.filter (fun f -> counts f && count f.before < count f.after) c.files)

let check_format c =
  let off s = List.exists (fun (t : Source.token) -> t.text = "ocamlformat") (Source.tokens s) in
  grew c (Option.fold ~none:false ~some:off) >? sprintf "%s switches ocamlformat off: let it format the code"

(** The lines by which definitions exceed the limit, summed, never rise. *)
let check_function_length c =
  let limit = c.config.max_function in
  let excess t = List.fold_left (fun s (_, _, n) -> s + n - limit) 0 (Lazy.force t.long) in
  let name (p, name, n) =
    if List.mem p (paths_of c.files) then Some (sprintf "`%s` (%s, %d code lines)" name p n) else None
  in
  if (not (changes_code c)) || excess c.tree <= excess c.parent_tree then []
  else
    [
      sprintf
        "code lines over the %d-line limit rise %d→%d in %s: split by decision; a definition that was already over the \
         limit shrinks in a `refactor` before it grows"
        limit (excess c.parent_tree) (excess c.tree)
        (String.concat ", " (List.filter_map name (Lazy.force c.tree.long)));
    ]

let check_copies c =
  let rise (folder, (a, b, where)) =
    when_ (b > a)
      (sprintf "copied lines in %s rise %d→%d (%s): make the copies one function, one table or one property" folder a b
         (String.concat ", " (spans where)))
  in
  List.concat_map rise (copies c)

(** No mutant disappears or changes unless the file it guards changed. *)
let check_mutants_kept c =
  let before = Lazy.force c.parent_tree.mutants and after = Lazy.force c.tree.mutants in
  let lost m = (not (List.mem (key m) (List.map key after))) && not (List.mem m.file (paths_of c.files)) in
  let n, k = (List.length before, List.length after) in
  List.map
    (fun m -> sprintf "the mutant for \"%s\" is gone, but %s did not change: keep it" m.why m.file)
    (List.filter lost before)
  @ when_ (k < n) (sprintf "mutants %d→%d: re-anchor a mutant whose line moved; never delete one" n k)

let checks =
  [
    check_message;
    check_budget;
    check_no_code;
    check_tested;
    check_expectations;
    check_files;
    check_parses;
    check_docs;
    check_labels;
    check_pure;
    check_format;
    check_function_length;
    check_copies;
    check_mutants_kept;
  ]

let problems c = List.concat_map (fun check -> check c) checks

let row (c, problems) =
  let b = budget c.config c.kind and net = net c in
  let dup f = match List.assoc_opt f (copies c) with Some (x, y, _) -> sprintf "%d→%d" x y | None -> "-" in
  let result = if problems = [] then "ok" else String.concat "<br>" problems in
  sprintf "| %s | %s | %+d / %+d | %+d / %+d | %s / %s | %s |" (String.sub c.sha 0 7) (Option.value c.kind ~default:"?")
    net.code b.code net.tests b.tests (dup "lib/") (dup "test/") result

(** The table of [shas], one row each and a total, and how many failed. *)
let table config shas =
  let step before sha =
    let c = make ?before config sha in
    (Some c.tree, (c, problems c))
  in
  let _, measured = List.fold_left_map step None shas in
  let total f = List.fold_left (fun n (c, _) -> n + f (net c)) 0 measured in
  let all =
    sprintf "| all %d | | %+d | %+d | | |" (List.length shas) (total (fun l -> l.code)) (total (fun l -> l.tests))
  in
  ( [ "| commit | type | code | tests | copied lib / test | result |"; "|---|---|---|---|---|---|" ]
    @ List.map row measured
    @ when_ (List.length shas > 1) all,
    List.length (List.filter (fun (_, ps) -> ps <> []) measured) )

let commits base head = lines (git [ "rev-list"; "--reverse"; "--no-merges"; base ^ ".." ^ head ])

(** Every commit of [base..head] against its parent, with the limits of [base]. *)
let base base head =
  let merges = lines (git [ "rev-list"; "--merges"; base ^ ".." ^ head ]) in
  let rows, failed = table (Config.at (At base)) (commits base head) in
  let merge sha =
    sprintf "| %s | merge | | | | a merge commit: rebase onto `%s` instead (linear history) |" (String.sub sha 0 7) base
  in
  let failed = failed + List.length merges in
  let fix = {|Fix each problem in the commit that has it. AGENTS.md, "Size and shape", explains every limit.|} in
  { title = "Size and shape"; rows = (rows @ List.map merge merges @ if failed > 0 then [ ""; fix ] else []); failed }

(** The working tree as one commit on top of HEAD with [message], made in a temporary index: HEAD, the index and the
    files stay as they are. *)
let worktree message =
  let config = Config.at (At "HEAD") in
  let commit tmp =
    let env = env_with [ ("GIT_INDEX_FILE", Filename.concat tmp "index") ] in
    ignore (git ~env [ "read-tree"; "HEAD" ]);
    ignore (git ~env ("add" :: "-A" :: "--" :: List.filter Sys.file_exists config.top_level));
    let tree = String.trim (git ~env [ "write-tree" ]) in
    String.trim (git [ "commit-tree"; tree; "-p"; "HEAD"; "-m"; message ])
  in
  let rows, failed = table config [ with_temp_dir commit ] in
  { title = "Size and shape (working tree)"; rows; failed }
