# caml-cwl-runner

A CWL v1.2.1 runner in OCaml 5.5. Binary: `ccr`.

These are the rules for changing this repo, for people and agents alike. They
win over an agent's own defaults, such as a shorter PR body, a TODO with
context or an attribution trailer. A rule that lives only in a chat, a
scratch file or one tool's memory binds no one: if it matters, it is written
here. The owner is the person who gives you the task; they decide and merge.
A rule marked [CI] is checked by CI on every PR. A rule without the mark
binds just as hard, and review checks it.

Read first: `README.md` (setup, use, exit codes), this file, and the `.mli` of
each module you change. `design_docs/runner.md` explains how the engine works;
read the section you need.

## Priorities

When rules pull apart, the higher one wins:

1. The CWL v1.2 spec (`vendor/cwl-v1.2`: `CommandLineTool.yml`,
   `Workflow.yml`, `Process.yml`, `invocation.md`) and Schema Salad
   (`vendor/cwl-v1.2/salad/schema_salad/metaschema`), then the v1.2
   conformance tests. cwltool decides only what the spec leaves open: match
   its behavior there, and never copy its code.
2. No conformance regression: a test that passes on `main` keeps passing.
3. The limits in Size and shape.
4. Everything else. A test, a doc comment or a mutant that a rule asks for is
   part of that rule, not of this item: when it doesn't fit a budget, make
   the change smaller.

If a higher rule can't be met without breaking a limit, stop. Don't weaken a
test, a gate or a limit to get through, and don't push the work. Say what
blocks you and by how much: in a draft PR's body, or in your last message
when you work unattended.

The bar for every line: someone with the current OCaml manual, the CWL spec
and some patience can understand and maintain it without help. Any feature of
stable OCaml is fair game; none is "too advanced".

## Code

Architecture:

- A function does I/O only through a module argument, and takes the narrowest
  signature that covers what it does: `(module Untyped_tree.FILE)` reads
  documents, `(module Glob.FS)` globs, `(module Runtime.READ)` inspects files,
  `(module Runtime.WRITE)` changes them, `(module Runtime.RUNTIME)` also
  spawns, and `(module Expr.ENGINE)` evaluates expressions.
- Only `lib/runtime.ml` and `bin/main.ml` do I/O (Eio, `Unix`, `Sys`). Inside
  `Runtime` every wait is an Eio operation; a blocking call with no Eio
  operation runs through `Eio_unix.run_in_systhread`. `Eio.Cancel.Cancelled`
  is re-raised, never turned into an `Error`.
- Types that several modules share, and module signatures, are defined once
  in `lib/data.ml` and `include`d by the public modules; a type that one
  module uses stays in it. No objects, no ppx.
- A failure is an `Error.t`. Build it with `Error.schema` (the process
  document is wrong, at a JSON path), `Error.runtime` (running failed: I/O, a
  collision, confinement), `Error.expr` (an expression failed) or
  `Error.unsupported` (valid CWL that ccr doesn't implement). `Parse`, `Type`
  and `Missing` have no constructor function, so build those by hand.
  Sequence results with `let*` and `let+` from `Error.Syntax`, and lists with
  `Error.map_list`, or `Schema.map_i` when the index is part of the path.
  Don't match an `Error` only to return it.
- Only `Untyped_tree` turns text into a tree. Read a document field with
  `Untyped_tree.string_field`, `bool_field` or `int_field`: a value of the
  wrong type is a schema error at its path, and `null` is absent. A JSON path
  starts from `""` at the document root and is built with the path helpers in
  `untyped_tree.mli` and `schema.mli` (`Schema.nth` for a list item). When a
  helper is missing, add it there rather than formatting a path inline.
- An unimplemented CWL construct is never dropped silently. When ccr can't run
  correctly without it, execution fails with `Unsupported` (exit 33);
  otherwise it is a diagnostic (`Error.diagnostic`), printed, and execution
  continues. A feature that runs part of a construct refuses the rest with
  `Unsupported`, in the same commit.
- ccr runs CWL itself. No code names or runs another CWL runner, such as
  cwltool or toil. [CI]
- Every File and Directory in an output is confined to the legal roots
  (`design_docs/runner.md`, Confinement). Never loosen that check.

Style:

- **One decision, one place.** Before writing a function, search (`rg`) for
  one that already makes that decision, and give it a parameter instead of
  writing a sibling. Two functions that differ in a noun, a field or a parser
  are one function.
- **Shorter wins.** Of two correct versions, keep the one with fewer code
  lines. A new helper must remove more code lines than it adds, counting its
  `.mli` line. Splitting a function that is over the limit is the exception.
- **Positional arguments.** Label an argument only when two arguments share a
  type and have no obvious order, when it is a boolean, or when it is a true
  optional that most callers omit (`?x`; an optional that callers routinely
  pass is a positional `'a option`). [CI: a new label in an `.mli` on a type
  that no other argument of the `val` has, other than `bool`]
- **Stdlib first.** Use what OCaml 5.5's Stdlib has (its `.mli` files are in
  `$(ocamlc -where)`) before writing a helper.
- **Name it, then branch.** Bind a call's result to a name that says what it
  is before matching on it, unless the patterns already say it.
- **Pure means pure.** Outside `lib/runtime.ml` and `bin/main.ml`: no I/O and
  no mutable state (`ref`, `:=`, `<-`, `mutable`, `Hashtbl`; a local `Buffer`
  is fine), and no `raise`, `failwith`, `assert false` or partial function
  (`List.hd`, `Option.get`, `String.sub`, ...) unless a comment
  `(* cannot raise: ... *)` on that line or the one above gives the reason in
  three or more words. [CI: the lines a commit changes, for the names in
  `IMPURE` and `RAISES` of `scripts/gates.py`]
- **Exhaustive where it decides.** A match that says how each kind of value is
  handled (a walk, a conversion, a printer) names every constructor, so a new
  one breaks the build where it must be handled. A `_` arm is fine for a
  yes/no question such as "is this a string?".
- **The obvious design.** Explicit APIs, validation at the boundary, no shims,
  fallbacks or compatibility layers. Delete a mechanism whose reason is gone.
- **No churn.** Every changed line serves the commit's purpose. Don't reorder,
  rename or reformat code you aren't otherwise changing. Code that breaks a
  rule is no precedent: never copy it, and fix it when a commit changes it.
- **Dead code goes**, in its own commit. No commented-out code. No TODO, FIXME
  or XXX [CI]: open a GitHub issue with the repro and the spec quote instead.
- **Dependencies** are the owner's call, and so is anything that isn't an
  opam package (C stubs, a vendored engine, a tool the tests run such as
  `node`): ask, with the reason.

## Tests

Alcotest only, with QCheck2 through `qcheck-alcotest`.

- Before writing a test, try to make the bug unrepresentable: a private type
  that only one function builds (`Schema.Container_outdir.t`), a narrower
  module argument, an exhaustive match. Then, in this order: one QCheck2
  property; one table with one row per case, run by one function. A lone
  hand-written case is the last resort.
- An expected value comes from the spec, the conformance suite, a law
  (identity, round trip) or a hand-checked literal, never from the code under
  test.
- A test asserts the specific outcome, never "any error" or "ok or error".
- A `feat` or `fix` that changes code comes with a quick test (marked
  `` `Quick ``) that fails without it: CI rebuilds the commit with its
  parent's `lib/` and `bin/`, and the quick tests must then fail [CI]. It also
  adds a mutant on a line it changed (Size and shape). When the whole
  behavior needs Docker, its pure part gets the quick test.
- A `refactor` adds no tests and changes no expectation [CI]. The existing
  tests, the mutants and the conformance baseline already pin the behavior.
- Never delete, skip or loosen a test to get green. A test case that
  disappears is named in the commit body as `suite: name`, the way
  `test_cwl.exe list` prints it, with what covers it now [CI]; the usual
  reason is a property or a table that checks as much. Two rewrites are not
  loosening, and the body says which: a test that pins a limitation
  (`Unsupported`, a diagnostic) changes in the `feat` that lifts it, and a
  test the spec contradicts changes in the `fix` that follows the spec, whose
  body quotes it.
- Alcotest covers internal contracts. CWL behavior is pinned by the
  conformance suite or a `test/oracle` case. An oracle case can't kill a
  mutant (`dune runtest` doesn't run it), so a `feat` or `fix` also needs an
  Alcotest case. `execute_edges` in `test/test_execute.ml` runs fixture tools
  end to end; its rows can share one tool and differ in the job.
- A property is named `prop_<law>`. A table is rows above one runner function,
  never a run of `match` blocks. Shared helpers live in `test/harness.ml`:
  reuse them. A new test module is registered in `test/test_cwl.ml`.
- A test that talks to Docker names its containers and images after its own
  temporary directory, so two runs on one machine don't collide.

Conformance is measured, not asserted:

- `scripts/conformance.py` runs cwltest on `vendor/cwl-v1.2`, and `--suite
  oracle` runs `test/oracle`. With `--baseline REF` it also runs the suite
  against ccr built at REF, and fails if a test that passes there no longer
  passes. Never keep a hand-written list of passing tests.
- A PR that makes conformance tests pass names their ids, with the spec
  section it implements, in the PR body. A `should_fail: true` test passes
  whenever ccr fails, for any reason: name it only when ccr fails for the
  reason the spec gives.
- An expected CWL output comes from the conformance suite or from a
  `test/oracle` case, never from memory. The case format and its authority
  rule (quote the spec, or record cwltool where the spec is silent) are in
  `scripts/oracle.py`.
- When cwltool or a v1.2 test contradicts the spec, the spec wins: the oracle
  case keeps the quoted output, the v1.2 test stays failing, and the PR quotes
  the spec. Don't bend ccr to match.
- Finding a failing test: `_build/conformance/<suite>.status` lists every id
  with its status. Read that test's tool and job and the one spec section it
  needs, not a whole spec file.

## Docs

One fact, one home; everywhere else links to it:

| Fact | Home |
|---|---|
| Rules for changing the repo | this file |
| The numbers behind Size and shape | `gates.json` |
| Setup, build, use, exit codes | `README.md` |
| How the engine works | `design_docs/runner.md` |
| What a module is for | the first comment of its `.mli` |
| What a function does, how it fails, why it exists | the doc comment after its `val` |
| What a type or constructor means | its doc comment where it is defined |
| Why a line is the way it is | a comment on that line |
| Which conformance tests pass | `_build/conformance/*.status`, generated |
| Feature gaps and open questions | GitHub issues |

Where two disagree, the spec wins, then the conformance and oracle cases, then
the `.mli`, then `runner.md`. Fix the one that is wrong in the same PR.

Comments don't count against Size and shape, so write them for a newcomer:

- Every `.mli`, and every `.ml` without one, opens with a `(** ... *)` comment
  saying what the module is for, and a new module of `lib/` has an `.mli`.
  [CI]
- Every `val` in an `.mli` has a doc comment after it: what it returns, how it
  fails, why it exists. Never restate the name or the type. [CI: a new `val`
  needs a doc comment of 4 or more words, and none loses one]
- Define a term before using it, and name the spec section you follow.
- A commit that changes a behavior or a contract changes its sentence in the
  same commit.

## Size and shape

These limits are hard. `scripts/gates.py` checks the ones marked [CI], commit
by commit, with the numbers in `gates.json`. CI runs the script and
`gates.json` as they are on `main`, so a PR that edits them is measured by the
old ones. Each failure it prints says how to fix it.

- [CI] **Budget.** Each commit is measured on its own, by its type. Code is
  the lines of OCaml files (`.ml`, `.mli`, `.mll`, `.mly`) outside `test/`
  that hold code; blank and comment lines are free. Tests are the same lines
  under `test/`, plus every non-blank line of `test/fixtures/`, comments
  included. `test/oracle` isn't counted: a case there holds a spec quote and
  an expected output, not logic. `budgets` in `gates.json` gives, for each
  type, the most code and test lines a commit may add, net; `docs`, `ci` and
  `chore` use `other`. A `refactor` never grows the code.
- **Splitting.** A feature bigger than its budget is split into commits that
  each hold a whole behavior, with its test and its mutant, and that each
  build and pass. Moving part of a behavior, its test or its mutant into the
  next commit to fit is a dodge, and so is moving logic into files the count
  skips. The plumbing a `feat` can't work without (an argument passed down, an
  export, a new module's `.mli`) belongs to that `feat`: name it in the body.
- [CI] **Honest types.** The types are `feat`, `fix`, `refactor`, `test`,
  `docs`, `ci` and `chore`. A `docs`, `ci` or `chore` commit changes no code
  (comments and layout may change). A `test` commit changes code only under
  `test/`. A `feat` or `fix` that changes code changes a test and adds a
  mutant on a line it changed. A `refactor` changes no test expectation: the
  literals of `test/*.ml` and the files under `test/fixtures/` and
  `test/oracle/` stay as they are.
- [CI] **No new copies.** Copied lines in `lib/` and in `test/` never
  increase. A copy is `window` code lines (`gates.json`) whose shape, with
  names and literals erased, appears again under the same folder; the rows of
  a table are not a copy. `scripts/gates.py --dups` lists them.
- [CI] **Short functions.** No definition (a `let`, nested ones included) in
  `lib/` or `bin/` is longer than `max_function` code lines (`gates.json`).
  The lines by which definitions exceed it, summed, never rise: one that is
  already over the limit shrinks, in a `refactor`, before a commit adds to it.
  Split by decision, one function per CWL rule; the pieces indent less, so a
  split usually saves lines.
- [CI] **Mutants.** A mutant is a file `test/mutants/<behavior>.json` holding
  `{"file", "old", "new", "why"}`. `file` is in `lib/` or `bin/`; `old`
  occurs exactly once in it, on one line (ocamlformat may re-wrap a longer
  one); `why` names the behavior. The gate copies the tree, swaps in `new`,
  which must still build, and the quick tests must then fail. A commit never
  removes a mutant, and changes one only when the file it guards changed in
  the same commit; then `old` moves to the line that now holds the behavior.
- [CI] **Messages.** The subject is `type(scope): Summary`, with a scope of
  lowercase letters, digits, `_`, `.` or `-`. The body says why, in the
  imperative, as the owner ("I", "we"). No `Co-authored-by`,
  `Signed-off-by` or "Generated with" line, and no names of tools, agents or
  conversations.
- [CI] **PR text.** The title is a commit subject. The body has the sections
  of `.github/pull_request_template.md`, filled in (How it works and OCaml
  worth noticing when the PR changes OCaml), and narrates no local test runs.
  The branch is `<type>/<short-slug>`, with a slug of lowercase letters,
  digits, `.`, `_` or `-`, and the base is `main`. This check reruns when the
  PR text is edited.
- [CI] **Linear history.** No merge commits in a PR; rebase instead.
- [CI] **Every commit builds** and is formatted, not just the last. No
  ocamlformat attribute switches the formatter off.
- [CI] **No stray files.** A new top-level file or directory must be listed
  in `gates.json`. No symbolic links.
- **The owner's files** are listed in `.github/CODEOWNERS`: the workflows,
  `AGENTS.md`, `gates.json`, `scripts/`, `vendor/`, every `dune` file,
  `dune-project` and `.ocamlformat`. A PR that changes one can't merge until
  the owner has read it, so keep such a change in its own PR, say why, and
  stack no other work on it. If a gate blocks correct work, stop
  (Priorities) and say which gate and why.

## Git and PRs

- **Chains.** Work in linear chains of PRs. Start a task from the tip of the
  open chain it belongs to (`git switch -c <type>/<slug> <tip>`), or from
  `origin/main` when none is open. Every PR targets `main`; the stack is the
  branch ancestry. Branch types are the commit types.
- **Bugs are fixed now.** A bug you find is never deferred and never fixed
  inside an unrelated commit. Branch the fix from `origin/main`, open its own
  `fix` PR, and rebase the open chain on top of it. A problem inside a PR of
  the chain is fixed in that PR; then rebase the PRs above it. A missing
  feature or an open question is a GitHub issue.
- **One change per PR, one reason per commit.** A behavior change never rides
  in a `refactor`. Each commit is the final shape of its change: no commit
  repairs another in the same PR.
- **Stage paths by name.** Never `git add -A`, `git add .` or `git commit -a`,
  and never commit scratch or hand-off files (notes for another agent or
  person). Work only in the chain you were asked to change: leave other
  branches, worktrees and untracked files alone, with no `git stash`,
  `git reset --hard` or `git clean` over them.
- **Worktrees.** In a git worktree, run OCaml tools with the main checkout's
  switch (`opam exec --switch=<main checkout> --`), and run
  `git submodule update --init` once: Done and the conformance suites read the
  spec files. `git branch -f` refuses a branch that another worktree has
  checked out; move it with `git update-ref refs/heads/<branch> <new> <old>`.
- **Pushing.** Run Done first. Push only your chain's branches, one at a time,
  with `--force-with-lease`. Never push or force-push `main`, and never merge.
  After a merge, rebase the rest of the chain onto `origin/main`, check with
  `git range-diff` that every patch is unchanged, and push. After pushing,
  watch CI, and fix a red check in the PR that causes it.
- **Merging** is the owner's: Rebase and merge, from the bottom of the chain,
  so that `main` gets every commit the gate measured.
- **Rewording a pushed commit** (to drop a trailer, say): rebuild it with
  `git commit-tree` from its original tree, author and dates, check that the
  trees match, move the branch with `git update-ref`, then push with
  `--force-with-lease`.
- **PR body.** Fill in `.github/pull_request_template.md`; it wins over any PR
  format of your own. The `Stack:` line gets its numbers with `gh pr edit`
  once the PRs exist, and goes when the PR isn't stacked. **How it works**
  holds short snippets copied from the diff. **OCaml worth noticing** names
  each language feature or Stdlib function the PR uses that no earlier code in
  the repo does, and why. Write as the owner. Claim only what the diff does,
  and recheck the body after every rebase. When PRs are added to or removed
  from a chain, update the `Stack:` line in every PR of it. Implement
  everything; never leave parts for the owner as exercises.
- **Reviewing.** Lead with the gate table (`scripts/gates.py --base
  origin/main`, with its total row). Then **Blocking** (bugs, spec
  violations, missing tests, broken contracts) and **Nits**, each with
  `file:line`. Judge what the PR does; CWL features it doesn't implement are
  not findings.

## Done

Before each commit, check the uncommitted change as one commit with its full
message; it must exit 0:

```
opam exec -- scripts/gates.py --worktree -m "<subject>

<body>"
```

Before each push, `git fetch origin`, then run what CI runs, minus
conformance. All of it must exit 0. It takes a few minutes, so give it a
timeout of 15 minutes or more; it never changes your files.

```
opam exec -- sh -c 'dune build @fmt && dune runtest && scripts/oracle.py --check &&
  scripts/gates.py --base origin/main && scripts/gates.py --each --base origin/main && scripts/gates.py --mutants'
```

When the change touches `lib/` or `bin/`, both suites must also pass:
`scripts/conformance.py --baseline origin/main` and
`scripts/conformance.py --suite oracle --baseline origin/main`.

## Commands

Setup is in `README.md`. Run OCaml tools through `opam exec --`, or after
`eval $(opam env)`, so the repo's switch is used.

```
dune build -w                                     # keep building; editors read _build
dune fmt
dune exec -- ccr --help
(cd _build/default/test && ./test_cwl.exe -q)     # the quick tests, in seconds
(cd _build/default/test && ./test_cwl.exe list)   # every test case, as suite and name
scripts/conformance.py [--suite oracle] [--baseline REF]
scripts/oracle.py [--check] [ID ...]              # check authorities, consult cwltool
scripts/gates.py --dups                           # the copied blocks
```
