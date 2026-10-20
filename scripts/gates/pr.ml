(** The PR's title, body and branches (AGENTS.md, "PR text"), read from the GitHub event that started the workflow. *)

open Repo
open Config

let sections = [ "**What**"; "**Why**"; "**How it works**"; "**OCaml worth noticing**" ]
let hint = Re.Perl.compile_pat ~opts:[ `Dotall ] {|<!--.*?-->|}
let placeholder = Re.Perl.compile_pat {|#[NMK]\b|\bK commits\b|}
let branch = Re.compile (Re.whole_string (Re.Perl.re ("(" ^ Commit.types ^ ")/[a-z0-9][a-z0-9._-]*")))

let narration =
  Re.Perl.compile_pat ~opts:[ `Caseless ]
    {|test plan|\b\d+ (tests?|cases?) (pass|passed|run|ran)\b|\b(ran|run|passed|tested) locally\b|\ball tests pass(ed)?\b|\bon my machine\b|}

(** The wanted sections of [body] that have nothing written under them. *)
let empty body wanted =
  let start s = Option.map (fun g -> (Re.Group.start g 0, s)) (Re.exec_opt (Re.compile (Re.str s)) body) in
  let found = List.sort compare (List.filter_map start sections) in
  let ends = List.map fst (List.tl (found @ [ (String.length body, "") ])) in
  let blank ((a, s), b) =
    List.mem s wanted && words (String.sub body (a + String.length s) (b - a - String.length s)) = []
  in
  List.map (fun ((_, s), _) -> "nothing is written under " ^ s) (List.filter blank (List.combine found ends))

(** The problems of the PR in the GitHub [event]. *)
let problems event =
  let pr = field "pull_request" event and head = field "head" (field "pull_request" event) in
  let title = string "title" pr and base = string "ref" (field "base" pr) and branch_name = string "ref" head in
  let raw = match field "body" pr with `String b -> b | _ -> "" in
  let body = Re.replace_string hint ~by:"" raw and default = string "default_branch" (field "repository" event) in
  let ocaml =
    match run "git" [ "diff"; "--name-only"; "origin/" ^ base ^ "..." ^ string "sha" head ] with
    | Some 0, out, _ -> List.exists Commit.ocaml (lines out)
    | _ -> true
  in
  let wanted = if ocaml then sections else List.filteri (fun i _ -> i < 2) sections in
  when_
    (not (Re.execp Commit.subject title))
    "the title is not `type(scope): Summary`: use the subject of the PR's main commit"
  @ when_
      (Re.execp Commit.leak (title ^ "\n" ^ raw))
      "the title or body names a tool or carries an attribution line: remove it"
  @ when_ (Re.execp narration body) "the body narrates local test runs: CI shows them, delete that"
  @ when_ (Re.execp placeholder body)
      "the Stack line still has the template's placeholders: fill it in, or delete it when the PR is not stacked"
  @ List.concat_map
      (fun s ->
        when_ (not (contains s body)) ("the body has no " ^ s ^ " section: fill in .github/pull_request_template.md"))
      wanted
  @ empty body wanted
  @ when_ (base <> default)
      (Printf.sprintf "the PR targets `%s`: every PR targets `%s`, and the stack is the branch ancestry" base default)
  @ when_
      (not (Re.execp branch branch_name))
      (Printf.sprintf
         "the branch `%s` is not `<type>/<short-slug>`, with a commit type and a slug of lowercase letters, digits, \
          `.`, `_` or `-`"
         branch_name)

(** The PR of the GitHub event in the file [path]. *)
let check path =
  let number, problems = decode path (fun e -> (int "number" (field "pull_request" e), problems e)) (slurp path) in
  let row p = Printf.sprintf "| PR #%d | %s |" number p in
  {
    title = "PR text";
    rows = "| check | result |" :: "|---|---|" :: List.map row (if problems = [] then [ "ok" ] else problems);
    failed = List.length problems;
  }
