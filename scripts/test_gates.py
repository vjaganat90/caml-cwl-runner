"""Tests for gates.py: every check catches what it should and passes what it
should, on commits made in a throwaway git repository. A fake `dune` stands
in for the real one where a check builds or runs tests.

  python3 -m unittest scripts/test_gates.py
"""

import contextlib
import io
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import gates  # noqa: E402

CONFIG = {
    "budgets": {
        "feat": {"code": 150, "tests": 60},
        "fix": {"code": 40, "tests": 40},
        "test": {"code": 0, "tests": 40},
        "refactor": {"code": 0, "tests": 0},
        "other": {"code": 0, "tests": 0},
    },
    "window": 3,
    "max_function": 3,
    "top_level": ["bin", "gates.json", "lib", "test"],
}
MUTANT = {"file": "lib/a.ml", "old": "x = x", "new": "x = 0", "why": "f is the identity"}
BASE = {
    "lib/a.mli": "(** A. *)\n\nval f : int -> int\n(** [f x] is [x] itself. *)\n",
    "lib/a.ml": "let f x = x\n",
    "test/t.ml": '(** Tests of A. *)\n\nlet name = "f is the identity"\nlet quote = \'"\'\nlet () = assert (A.f 1 = 1)\n',
    "test/fixtures/x.cwl": "class: CommandLineTool\n",
    "test/mutants/f.json": json.dumps(MUTANT),
    "gates.json": json.dumps(CONFIG),
}
WHY = "\n\nThe reason."
TEST = {"test/t.ml": BASE["test/t.ml"] + "let () = assert (A.f 0 = 0)\n"}
NEW_MUTANT = {"test/mutants/g.json": json.dumps({"file": "lib/a.ml", "old": "+ 0", "new": "+ 1", "why": "f adds nothing"})}
FIX = {**TEST, **NEW_MUTANT}  # a feat or fix whose changed line of lib/a.ml holds "+ 0"
LONG = "let f x =\n  let y = x + 0 in\n  let z = y in\n  z\n"  # 4 code lines, over the limit of 3

# name, files changed (None deletes), commit message, a phrase the problems
# must contain (None: no problem at all)
CASES = [
    ("refactor that shrinks", {"lib/a.ml": "let f = Fun.id\n"}, "refactor: Shrink f" + WHY, None),
    ("refactor that grows", {"lib/a.ml": "let f x =\n  x\n"}, "refactor: Grow f" + WHY, "must not grow"),
    ("code outside lib counts", {"bin/main.ml": "(** Main. *)\n\nlet () = ()\n"}, "refactor: Add main" + WHY, "must not grow"),
    ("feat within budget", {"lib/a.ml": "let f x = x\nlet g x = x + 0\n", **FIX}, "feat: Add g" + WHY, None),
    ("tests over budget", {"test/t.ml": BASE["test/t.ml"] + "let () = ()\n" * 41}, "test: Add many" + WHY, "tests +41 over +40"),
    ("fixture lines are tests", {"test/fixtures/x.cwl": "a\n" * 42}, "test: Add a fixture" + WHY, "tests +41 over +40"),
    ("binary fixtures count nothing", {"test/fixtures/x.bin": "\0\n" * 50}, "test: Add a blob" + WHY, None),
    ("docs changing a comment", {"lib/a.ml": "(* f is the identity *)\nlet f x = x\n"}, "docs: Explain f" + WHY, None),
    ("docs changing code", {"lib/a.ml": "let f x = x + 0\n"}, "docs: Explain f" + WHY, "changes code in lib/a.ml"),
    ("docs changing a string", {"test/t.ml": BASE["test/t.ml"].replace("identity", "id")}, "docs: Reword" + WHY, "changes code in test/t.ml"),
    ("test touching lib", {"lib/a.ml": "let f x = x + 0\n", **TEST}, "test: Sneak" + WHY, "changes code in lib/a.ml"),
    ("fix without a test", {"lib/a.ml": "let f x = x + 0\n", **NEW_MUTANT}, "fix: Change f" + WHY, "needs a test"),
    ("fix with a comment as its test", {"lib/a.ml": "let f x = x + 0\n", "test/t.ml": BASE["test/t.ml"] + "(* note *)\n", **NEW_MUTANT},
     "fix: Change f" + WHY, "needs a test"),
    ("fix without a mutant", {"lib/a.ml": "let f x = x + 0\n", **TEST}, "fix: Change f" + WHY, "adds a mutant"),
    ("fix whose mutant is on a line it did not change", {"lib/a.ml": "let f x = x\nlet h = 2 + 0\n", **TEST,
     "test/mutants/g.json": json.dumps({**MUTANT, "new": "x = 5"})}, "fix: Add h" + WHY, "adds a mutant"),
    ("fix whose mutant is in a test", {"lib/a.ml": "let f x = x + 0\n", **TEST,
     "test/mutants/g.json": json.dumps({"file": "test/t.ml", "old": "A.f 0 = 0", "new": "A.f 0 = 1", "why": "t"})},
     "fix: Change f" + WHY, "adds a mutant"),
    ("fix with a test and a mutant", {"lib/a.ml": "let f x = x + 0\n", **FIX}, "fix: Change f" + WHY, None),
    ("refactor changing an expectation", {"test/t.ml": BASE["test/t.ml"].replace("identity", "id")},
     "refactor: Rename" + WHY, "changes test expectations"),
    ("refactor changing a number", {"test/t.ml": BASE["test/t.ml"].replace("A.f 1 = 1", "A.f 1 = 2")},
     "refactor: Tidy" + WHY, "changes test expectations"),
    ("refactor renaming in a test", {"test/t.ml": BASE["test/t.ml"].replace("let quote", "let q")}, "refactor: Rename" + WHY, None),
    ("refactor touching an oracle case", {"test/oracle/c.yml": "x: 1\n"}, "refactor: Move" + WHY, "changes test expectations"),
    ("subject not conventional", {"lib/a.ml": "let f = Fun.id\n"}, "Shrink f" + WHY, "not `type(scope): Summary`"),
    ("perf is not a type", {"lib/a.ml": "let f = Fun.id\n"}, "perf: Shrink f" + WHY, "not `type(scope): Summary`"),
    ("revert is not a type", {"lib/a.ml": "let f = Fun.id\n"}, "revert: Shrink f" + WHY, "not `type(scope): Summary`"),
    ("a module name as scope", {"lib/a.ml": "let f = Fun.id\n"}, "refactor(command_line_tool.v1): Shrink f" + WHY, None),
    ("attribution trailer", {"lib/a.ml": "let f = Fun.id\n"}, "refactor: Shrink f" + WHY + "\n\nMade-with: An editor", "found `Made-with:`"),
    ("generated line", {"lib/a.ml": "let f = Fun.id\n"}, "refactor: Shrink f" + WHY + "\n\nGenerated with an assistant.", "found `Generated with`"),
    ("tool named", {"lib/a.ml": "let f = Fun.id\n"}, "refactor: Shrink f as discussed with Grok" + WHY, "names a tool"),
    ("a cursor is a word", {"lib/a.ml": "let f = Fun.id\n"}, "refactor: Move the YAML cursor" + WHY, None),
    ("no body", {"lib/a.ml": "let f = Fun.id\n"}, "refactor: Shrink f", "no body"),
    ("failwith in pure code", {"lib/a.ml": 'let f x = if x < 0 then failwith "neg" else x + 0\n', **FIX},
     "fix: Reject negatives" + WHY, "calls `failwith`"),
    ("justified raise", {"lib/a.ml": "(* cannot raise: [x] is not empty *)\nlet f x = List.hd [ x + 0 ]\n", **FIX},
     "fix: Use hd" + WHY, None),
    ("a raise with no reason", {"lib/a.ml": "(* cannot raise *)\nlet f x = List.hd [ x + 0 ]\n", **FIX},
     "fix: Use hd" + WHY, "calls `List.hd`"),
    ("a reason in a string", {"lib/a.ml": 'let f x = ignore "cannot raise: it is fine"; List.hd [ x + 0 ]\n', **FIX},
     "fix: Use hd" + WHY, "calls `List.hd`"),
    ("raise in a string or comment", {"lib/a.ml": '(* failwith *)\nlet f x = ignore "failwith"; x + 0\n', **FIX},
     "fix: Note" + WHY, None),
    ("words in a quoted string", {"lib/a.ml": "let f x = x + 0\nlet js = {js|\nprocess.exit(1); var ref = 0;\n|js}\n", **FIX},
     "fix: Keep the prelude" + WHY, None),
    ("I/O in pure code", {"lib/a.ml": 'let f x = ignore (Sys.getenv "X"); x + 0\n', **FIX}, "fix: Read X" + WHY, "uses `Sys`"),
    ("an alias of an I/O module", {"lib/a.ml": "module H = Hashtbl\nlet f x = x + 0\n", **FIX}, "fix: Alias" + WHY, "uses `Hashtbl`"),
    ("a channel", {"lib/a.ml": 'let f x = ignore (open_in "x"); x + 0\n', **FIX}, "fix: Open" + WHY, "uses `open_in`"),
    ("mutable state", {"lib/a.ml": "type c = { mutable n : int }\nlet f x = x + 0\n", **FIX}, "fix: Count" + WHY, "uses `mutable`"),
    ("a ref", {"lib/a.ml": "let f x = let r = ref (x + 0) in !r\n", **FIX}, "fix: Box x" + WHY, "uses `ref`"),
    ("lib/runtime.ml may do I/O", {"lib/runtime.mli": "(** Runtime. *)\n\nval env : unit -> string\n(** [env ()] is the variable X, read now. *)\n",
                                   "lib/runtime.ml": 'let env () = Sys.getenv "X"\n', "lib/a.ml": "let f x = x + 0\n", **FIX},
     "feat: Read X" + WHY, None),
    ("another runner", {"lib/a.ml": 'let f x = ignore "cwltool"; x + 0\n', **FIX}, "feat: Hand off" + WHY, "names another CWL runner"),
    ("ocamlformat switched off", {"lib/a.ml": 'let f x = x + 0 [@@ocamlformat "disable"]\n', **FIX}, "fix: Pack f" + WHY,
     "switches ocamlformat off"),
    ("code hidden in an extension string", {"lib/a.ml": "let f x = x\n[@@@a {%x|(*|}]\nlet g x = x\n[@@@a {%x|*)|}]\n"},
     "refactor: Hide g" + WHY, "must not grow"),
    ("a TODO", {"lib/a.ml": "(* Todo: later *)\nlet f x = x\n"}, "docs: Plan" + WHY, "TODO"),
    ("stray top-level file", {"notes.md": "scratch\n"}, "docs: Add notes" + WHY, "at the top level"),
    ("mli without a header", {"lib/b.mli": "val g : int\n(** One more value here. *)\n", "lib/b.ml": "let g = 1\n"},
     "feat: Add B" + WHY, "must open with"),
    ("a new module without an mli", {"lib/b.ml": "(** B. *)\n\nlet g = 1\n"}, "feat: Add B" + WHY, "lib/b.ml has no .mli"),
    ("undocumented val", {"lib/a.mli": BASE["lib/a.mli"] + "\nval g : int\n", "lib/a.ml": "let f x = x\nlet g = 1 + 0\n", **FIX},
     "feat: Add g" + WHY, "no doc comment of 4 or more words after lib/a.mli: g"),
    ("a stub doc comment", {"lib/a.mli": BASE["lib/a.mli"] + "\nval g : int\n(** G. *)\n", "lib/a.ml": "let f x = x\nlet g = 1 + 0\n", **FIX},
     "feat: Add g" + WHY, "no doc comment of 4 or more words"),
    ("a doc comment that wraps onto a keyword", {"lib/a.mli": BASE["lib/a.mli"] + "\nval g : int\n(** [g] is one more than the\n    type of nothing, in words. *)\n",
                                                 "lib/a.ml": "let f x = x\nlet g = 1 + 0\n", **FIX}, "feat: Add g" + WHY, None),
    ("a lonely label", {"lib/a.mli": BASE["lib/a.mli"] + "\nval g : path:string -> int -> int\n(** [g ~path n] is n, whatever the path. *)\n",
                        "lib/a.ml": "let f x = x\nlet g ~path n = ignore path; n + 0\n", **FIX}, "feat: Add g" + WHY, "a label is only"),
    ("labels on two strings", {"lib/a.mli": BASE["lib/a.mli"] + "\nval g : a:string -> b:string -> int\n(** [g ~a ~b] is the sum of their lengths. *)\n",
                               "lib/a.ml": "let f x = x\nlet g ~a ~b = String.length a + String.length b + 0\n", **FIX}, "feat: Add g" + WHY, None),
    ("an optional and a bool label", {"lib/a.mli": BASE["lib/a.mli"] + "\nval g : ?n:int -> quiet:bool -> int -> int\n(** [g ?n ~quiet x] is x, louder or not. *)\n",
                                      "lib/a.ml": "let f x = x\nlet g ?(n = 0) ~quiet x = ignore (n, quiet); x + 0\n", **FIX}, "feat: Add g" + WHY, None),
    ("a function at the limit", {"lib/a.ml": "let f x =\n  let y = x + 0 in\n  y\n", **FIX}, "fix: Spell f out" + WHY, None),
    ("long function", {"lib/a.ml": LONG, **FIX}, "fix: Spell f out" + WHY, "`f` (lib/a.ml, 4 code lines)"),
    ("long function in a module", {"lib/a.ml": "module M = struct\n" + "".join("  " + l + "\n" for l in LONG.splitlines()) + "end\n", **FIX},
     "fix: Nest f" + WHY, "`f` (lib/a.ml, 4 code lines)"),
    ("long let ()", {"lib/a.ml": LONG.replace("let f x", "let ()"), **FIX}, "fix: Run f" + WHY, "`()` (lib/a.ml, 4 code lines)"),
    ("long function shadowed", {"lib/a.ml": LONG + "let f x = x\n", **FIX}, "fix: Shadow f" + WHY, "`f` (lib/a.ml, 4 code lines)"),
    ("new copy in lib", {"lib/a.ml": "let f x =\n  let y = x in\n  y\n\nlet g z =\n  let w = z in\n  w\n\nlet h = 1 + 0\n", **FIX},
     "feat: Add g" + WHY, "copied lines in lib/ rise"),
    ("new copy in a subfolder", {"lib/b/c.ml": "(** C. *)\n\nlet f x =\n  let y = x in\n  y\n\nlet g z =\n  let w = z in\n  w\n"},
     "refactor: Add c" + WHY, "copied lines in lib/ rise"),
    ("new copy in test", {"test/t.ml": BASE["test/t.ml"] + "let a () =\n  let b = 1 in\n  b\n\nlet c () =\n  let d = 2 in\n  d\n"},
     "test: Add two" + WHY, "copied lines in test/ rise"),
    ("one repeated line is a table", {"test/t.ml": BASE["test/t.ml"] + "let rows =\n" + '  [ "a";\n' * 8 + "  ]\n"},
     "test: Add rows" + WHY, None),
    ("mutant swapped", {"test/mutants/f.json": json.dumps({**MUTANT, "new": "x = 2"})}, "test: Swap" + WHY, "is gone"),
    ("mutant moved with its line", {"lib/a.ml": "let f y = y\n", "test/mutants/f.json": json.dumps({**MUTANT, "old": "y = y", "new": "y = 0"})},
     "refactor: Rename x" + WHY, None),
    ("mutant deleted", {"test/mutants/f.json": None}, "test: Drop it" + WHY, "mutants 1→0"),
]


def git(*args):
    return subprocess.run(["git", *args], capture_output=True, text=True, check=True).stdout


def write(files):
    for path, text in files.items():
        if text is None:
            os.remove(path)
        else:
            Path(path).parent.mkdir(parents=True, exist_ok=True)
            Path(path).write_text(text)


def commit(files, message):
    write(files)
    git("add", "-A")
    git("commit", "-q", "--allow-empty", "-m", message)
    return git("rev-parse", "HEAD").strip()


def quietly(f, *args):
    out = io.StringIO()
    with contextlib.redirect_stdout(out):
        return f(*args), out.getvalue()


class Gates(unittest.TestCase):
    def setUp(self):
        self.cwd, self.dir = os.getcwd(), tempfile.TemporaryDirectory()
        self.env = dict(os.environ)
        # The tester's own git config, hooks and signing stay out.
        os.environ.update(GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@t", GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@t",
                          GIT_CONFIG_GLOBAL=os.devnull, GIT_CONFIG_NOSYSTEM="1")
        os.chdir(self.dir.name)
        git("init", "-q", "-b", "main")
        self.base = commit(BASE, "chore: Start" + WHY)
        gates.read.cache_clear()

    def tearDown(self):
        os.chdir(self.cwd)
        os.environ.clear()
        os.environ.update(self.env)
        self.dir.cleanup()

    def problems(self, files, message):
        c = gates.Commit(commit(files, message), CONFIG)
        return [p for check in gates.CHECKS for p in check(c)]

    def fake_dune(self, script):
        """Put a `dune` that runs `script` (sh) first on the PATH."""
        Path("bin-fake").mkdir()
        Path("bin-fake/dune").write_text("#!/bin/sh\n" + script)
        os.chmod("bin-fake/dune", 0o755)
        os.environ["PATH"] = f"{self.dir.name}/bin-fake:{os.environ['PATH']}"
        Path(".git/info/exclude").write_text("bin-fake\n")

    def run_gates(self, *args, cwd="."):
        return subprocess.run([sys.executable, gates.__file__, *args], cwd=cwd, capture_output=True, text=True)

    def test_cases(self):
        for name, files, message, expect in CASES:
            with self.subTest(name):
                git("reset", "-q", "--hard", self.base)
                git("clean", "-qfd")
                found = self.problems(files, message)
                if expect is None:
                    self.assertEqual(found, [])
                else:
                    self.assertTrue(any(expect in p for p in found), found)

    def test_long_function_renamed(self):
        commit({"lib/a.ml": LONG}, "feat: Spell f out" + WHY)
        self.assertEqual(self.problems({"lib/a.ml": LONG.replace("let f", "let g")}, "refactor: Rename f" + WHY), [])
        self.assertTrue(any("rise 1→2" in p for p in self.problems({"lib/a.ml": LONG.replace("z\n", "z\n  |> Fun.id\n")}, "refactor: Grow f" + WHY)))

    def test_total(self):
        start = commit({"lib/a.ml": "let f x = x\nlet g x = x\nlet h x = x\n"}, "feat: Add g and h" + WHY)
        commit({"lib/a.ml": "let f x = x\n"}, "refactor: Drop g and h" + WHY)
        commit({"test/t.ml": BASE["test/t.ml"] + "let () = ()\n"}, "test: Add one" + WHY)
        self.assertIn("| all 2 | | -2 | +1 |", quietly(gates.check_base, start, "HEAD")[1])

    def test_symbolic_link(self):
        os.symlink("../test/fixtures/x.cwl", "lib/b.ml")
        self.assertTrue(any("symbolic link" in p for p in self.problems({}, "refactor: Link b" + WHY)))

    def test_merge_commit(self):
        git("checkout", "-q", "-b", "side")
        commit({"lib/a.ml": "let f = Fun.id\n"}, "refactor: Shrink f" + WHY)
        git("checkout", "-q", "-")
        commit({"test/t.ml": BASE["test/t.ml"] + "let () = ()\n"}, "test: Add one" + WHY)
        git("merge", "-q", "--no-ff", "side", "-m", "Merge side")
        self.assertEqual(quietly(gates.check_base, self.base, "HEAD")[0], 1)

    def test_bad_base(self):
        with self.assertRaises(SystemExit):
            quietly(gates.check_base, "no-such-ref", "HEAD")

    def test_worktree(self):
        write({"lib/a.ml": "let f x =\n  x\n", "notes.md": "untracked scratch\n"})
        self.assertEqual(quietly(gates.check_worktree, "refactor: Grow f" + WHY)[0], 1)
        self.assertEqual(git("rev-parse", "HEAD").strip(), self.base)
        self.assertEqual(self.run_gates("--worktree", "-m", "refactor: Grow f" + WHY, cwd="lib").returncode, 1)  # from a subfolder too
        self.assertEqual(self.run_gates("--worktree").returncode, 2)  # the message decides the budget
        write({"lib/a.ml": "let f x = x + 1\n"})
        self.assertEqual(quietly(gates.check_worktree, "feat: Shift f" + WHY)[0], 1)  # a feat needs a test and a mutant

    def test_each(self):
        # The fake build lists test/names.txt as the test cases; the fake tests
        # pass when lib/a.ml holds the text of test/expect.txt.
        self.fake_dune('case "$1" in\n'
                       'build) grep -q BROKEN lib/a.ml && exit 1\n'
                       '  mkdir -p _build/default/test\n'
                       '  printf \'#!/bin/sh\\nsed "s/^/t 0 /;s/$/./" test/names.txt\\n\' > _build/default/test/test_cwl.exe\n'
                       '  chmod +x _build/default/test/test_cwl.exe; exit 0 ;;\n'
                       'runtest) [ -f test/expect.txt ] && ! grep -qF "$(cat test/expect.txt)" lib/a.ml && exit 1; exit 0 ;;\n'
                       'esac\nexit 0\n')
        start = commit({"test/names.txt": "one\ntwo\n"}, "test: Name the cases" + WHY)
        commit({"lib/a.ml": "let f x = x (* BROKEN *)\n"}, "refactor: Break" + WHY)
        commit({"lib/a.ml": "let f x = x\n", "test/names.txt": "one\n"}, "test: Drop a case" + WHY)
        commit({"lib/a.ml": "let f x = x + 0\n", "test/expect.txt": "x + 0", **FIX}, "fix: Add zero" + WHY)
        commit({"lib/a.ml": "let f x = x + 0 + 0\n", **TEST, "test/mutants/h.json": json.dumps({**MUTANT, "old": "0 + 0", "new": "0"})},
               "fix: Add more" + WHY)
        failed, out = quietly(gates.check_each, start, "HEAD")
        self.assertEqual(failed, 3, out)  # the broken build, the dropped case, the fix its tests don't notice
        self.assertIn("gone: t: two", out)
        self.assertIn("pass with the parent's code", out)
        self.assertEqual(Path("lib/a.ml").read_text(), "let f x = x + 0 + 0\n")  # the checkout never moved
        self.assertEqual(len(git("worktree", "list").splitlines()), 1)

    def test_mutants(self):
        self.fake_dune('[ "$1" = runtest ] && grep -q "x = 0" lib/a.ml && exit 1; exit 0\n')
        write({"test/mutants/g.json": json.dumps({**MUTANT, "new": "x = 5", "why": "survivor"}),
               "test/mutants/h.json": json.dumps({**MUTANT, "old": "nowhere", "why": "stale"}),
               "test/mutants/i.json": json.dumps({**MUTANT, "file": "test/t.ml", "why": "a test"})})
        failed, out = quietly(gates.check_mutants)
        self.assertEqual(failed, 3, out)
        self.assertIn("survives", out)
        self.assertIn("found 0 times", out)
        self.assertIn("not a file in lib/ or bin/", out)
        self.assertEqual(Path("lib/a.ml").read_text(), BASE["lib/a.ml"])  # mutants touch a scratch copy only

    def test_mutants_need_passing_tests(self):
        self.fake_dune('[ "$1" = runtest ] && exit 1; exit 0\n')
        failed, out = quietly(gates.check_mutants)
        self.assertEqual(failed, 1)
        self.assertIn("with no mutant the code is fail", out)

    def test_code(self):
        src = 'let a = "(* not a comment" (* a (* nested *) "*)" *) let b = \'"\'\nlet c = {id|*)|id}\n(* only *)\n'
        self.assertEqual([n for n, _ in gates.code_lines(src)], [1, 2])
        self.assertEqual(gates.shape('let x = f "s" 12'), gates.shape('let y = g "t" 3'))
        self.assertEqual(len(gates.code_lines("let s = {|\na\nb\n|}\n")), 4)
        self.assertEqual(len(gates.code_lines("[@@@h {%a|(*|}]\nlet x = 1\n[@@@h {%a|*)|}]\n")), 3)
        self.assertEqual(gates.bare('let s = "ref" ^ {|\nexit\n|}\n'), 'let s = "" ^ ""\n\n\n')
        self.assertTrue(gates.tabular(["a", "b"] * 5))
        self.assertFalse(gates.tabular(list("abcdefghij")))

    def test_pr(self):
        git("update-ref", "refs/remotes/origin/main", self.base)
        ocaml = commit({"lib/a.ml": "let f = Fun.id\n"}, "refactor: Shrink f" + WHY)
        body = "**What**\n- x\n\n**Why** y\n\n**How it works**\nz\n\n**OCaml worth noticing**\n- z\n"
        short = "**What**\n- x\n\n**Why** y\n"
        template = (Path(__file__).parent.parent / ".github/pull_request_template.md").read_text()
        event = {"pull_request": {"number": 1, "title": "fix: Do x", "body": body, "base": {"ref": "main"},
                                  "head": {"ref": "fix/do-x.v2", "sha": ocaml}}, "repository": {"default_branch": "main"}}
        for change, expect in [({}, 0), ({"title": "Do x"}, 1), ({"body": body + "\nTest plan: 112 tests pass"}, 1),
                               ({"body": short}, 2), ({"body": short + "There's no OCaml in this PR."}, 2),
                               ({"body": short, "head": {"ref": "fix/do-x", "sha": self.base}}, 0),
                               ({"body": body.replace("z\n\n", "<!-- hint -->\n\n")}, 1), ({"body": template}, 5),
                               ({"base": {"ref": "fix/other"}}, 1), ({"head": {"ref": "my-branch", "sha": ocaml}}, 1),
                               ({"body": body + "\n\U0001F916 Generated"}, 1)]:
            Path("event.json").write_text(json.dumps({**event, "pull_request": {**event["pull_request"], **change}}))
            self.assertEqual(quietly(gates.check_pr, "event.json")[0], expect, change)


if __name__ == "__main__":
    unittest.main()
