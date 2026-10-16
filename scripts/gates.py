#!/usr/bin/env python3
"""The hard limits of AGENTS.md ("Size and shape"), checked commit by commit.

  gates.py --base REF [--head REV]  Check every commit in REF..REV (default
                                    HEAD) against its parent.
  gates.py --worktree -m MESSAGE    Check the uncommitted changes as one
                                    commit with that message, before you
                                    commit them.
  gates.py --each --base REF [--head REV]
                                    Build every commit of the range: it is
                                    formatted, its message names each test
                                    case it removes, and a feat or fix has
                                    quick tests that fail on its parent.
  gates.py --mutants                Every mutant in test/mutants/ builds and
                                    makes the quick tests fail.
  gates.py --dups                   List the copied blocks in lib/ and test/,
                                    as the files are on disk now.
  gates.py --pr EVENT_JSON          Check a PR's title, body and branches.

Every failure says how to fix it, and the script exits 1. It runs from any
directory of the repository. --each and --mutants work in a scratch copy, so
the files you work on never change. CI runs this script and reads gates.json
as they are on the base branch, so a PR that edits them is measured by the
old ones, and .github/CODEOWNERS makes the owner read any change to them or
to the workflows that run them.

A mutant is a file test/mutants/<behavior>.json holding {"file", "old",
"new", "why"}: `old` occurs exactly once in `file`; the gate swaps in `new`,
which must build, and the quick tests (the Alcotest cases marked `Quick)
must then fail. A report row reads: commit, type, code lines added / budget,
test lines added / budget, copied lines in lib/ and test/ (before -> after),
and the problems found.
"""

import argparse
import functools
import json
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
from collections import Counter
from pathlib import Path

KEYWORDS = set(
    "and as assert begin class constraint do done downto else end exception"
    " external false for fun function functor if in include inherit"
    " initializer lazy let match method module mutable new nonrec object of"
    " open or private rec sig struct then to true try type val virtual when"
    " while with".split()
)
TYPES = "feat|fix|refactor|test|docs|ci|chore"
SUBJECT = re.compile(rf"({TYPES})(\([a-z0-9_.-]+\))?!?: \S")
BRANCH = re.compile(rf"({TYPES})/[a-z0-9][a-z0-9._-]*")
# A trailer such as Co-authored-by: or Made-with:, a "Generated with" line, a
# robot, or a tool or a conversation named in a commit message or a PR.
LEAK = re.compile(
    r"(?im)^[\w-]+-(by|with):|^generated (with|by)\b|\U0001F916"
    r"|\b(grok|claude|copilot|chatgpt|codex|gemini|windsurf|aider|as discussed|per (the )?review|future sessions?)\b"
)
# Calls that raise. Pure code returns Error.t instead, or says in a comment
# on the line, or on the line above, why the call "cannot raise".
RAISES = re.compile(
    r"\b(raise|raise_notrace|failwith|invalid_arg|exit|Option\.get|List\.(hd|tl|nth"
    r"|find|assoc|assq|map2|iter2|fold_left2|fold_right2|for_all2|exists2|combine)"
    r"|Result\.get_(ok|error)|Hashtbl\.find|String\.(sub|index|rindex|index_from)|Bytes\.sub"
    r"|(int|float|bool)_of_string|Int64\.of_string|Char\.chr)\b|\bassert\s+false\b"
)
# I/O and mutable state, which only lib/runtime.ml and bin/main.ml may use. A
# module counts wherever it is named, so an alias or an `open` of it counts too.
IMPURE = re.compile(
    r"\b(Sys|Unix|Eio|Eio_unix|Eio_main|In_channel|Out_channel|Lwt|Hashtbl|Obj|Queue|Stack|Atomic"
    r"|Mutex|Condition|Domain|Thread|Random)\b|\bFilename\.(temp_\w+|open_temp_file)"
    r"|\b(print_\w+|prerr_\w+|read_(line|int|float)|(open|close)_(in|out)\w*|input_(line|char|byte)"
    r"|output_(string|char|bytes|substring)|really_input\w*|flush(_all)?|at_exit|ref|incr|decr|mutable)\b"
    r"|\b(Printf|Format)\.e?printf\b|\bPrintf\.fprintf\b|:=|<-"
)
# Another CWL runner. ccr runs CWL itself and never hands work to one.
RUNNERS = re.compile(r"(?i)\b(cwltool|cwl-runner|toil|cromwell|nextflow|snakemake|arvados|cwltest)\b")
STRING = r'"(?:\\.|[^"\\])*"'
# {id|...|id}, and {%ext|...|} or {%ext id|...|id} with an extension name.
QUOTED = r"\{(?:%%?[A-Za-z_][\w'.]*[ \t]*)?(?P<id>[a-z_]*)\|.*?\|(?P=id)\}"
CHAR = r"(?<![\w'])'(?:\\(?:\d{3}|x[0-9a-fA-F]{2}|o[0-7]{3}|.)|[^\\'\n])'"
LITERAL = re.compile(rf"{STRING}|{QUOTED}|{CHAR}", re.S)
LEXEME = re.compile(rf"\(\*|\*\)|{STRING}|{QUOTED}|{CHAR}", re.S)
EXPECTED = re.compile(rf"{STRING}|{QUOTED}|{CHAR}|\b\d[\w.]*|\b(?:true|false)\b", re.S)
TOKEN = re.compile(rf"{STRING}|\w[\w']*|\S")
DEF = re.compile(r"( *)(?:let|and)\b(?:\[@[^\]]*\])?(?: +rec\b)? *([a-z_][\w']*|\(.*?\))?")
OCAML = (".ml", ".mli", ".mll", ".mly")
SECTIONS = ("**What**", "**Why**", "**How it works**", "**OCaml worth noticing**")
NARRATION = re.compile(
    r"(?i)test plan|\b\d+ (tests?|cases?) (pass|passed|run|ran)\b|\b(ran|run|passed|tested) locally\b"
    r"|\ball tests pass(ed)?\b|\bon my machine\b"
)


def git(*args, env=None):
    r = subprocess.run(["git", *args], capture_output=True, text=True, errors="replace", env=env)
    if r.returncode:
        sys.exit(f"git {' '.join(args)} failed (a missing ref may need `git fetch origin`): {r.stderr.strip()}")
    return r.stdout


@functools.cache
def read(rev, path):
    """The file `path` at `rev` (on disk when `rev` is None), or None."""
    if rev is None:
        return Path(path).read_text(errors="replace") if Path(path).is_file() else None
    r = subprocess.run(["git", "show", f"{rev}:{path}"], capture_output=True, text=True, errors="replace")
    return r.stdout if r.returncode == 0 else None


def paths(rev, folder):
    """The files under `folder` at `rev` (on disk when `rev` is None)."""
    if rev is None:
        return sorted(str(p) for p in Path(folder).rglob("*") if p.is_file())
    return git("ls-tree", "-r", "--name-only", rev, folder).split("\n")[:-1]


@functools.cache
def touched(parent, sha, path):
    """The line numbers of `path` at `sha` that differ from `parent`; a
    deletion touches the lines on either side of it."""
    out = set()
    diff = git("diff", "-U0", "--no-renames", parent, sha, "--", path)
    for m in re.finditer(r"^@@ -\S+ \+(\d+)(?:,(\d+))? @@", diff, re.M):
        start, n = int(m[1]), int(m[2] or 1)
        out.update(range(start, start + n) if n else (start, start + 1))
    return out


# Reading OCaml ---------------------------------------------------------------


@functools.cache
def code(src):
    """OCaml `src` with every comment removed and line breaks kept, so line n
    of the result is line n of `src`. Literals are lexed whole, so a "(*"
    inside a string opens no comment."""
    out, depth, end = [], 0, 0

    def keep(text):  # inside a comment only the line breaks stay
        out.append("\n" * text.count("\n") if depth else text)

    for m in LEXEME.finditer(src):
        if m[0] == "*)" and not depth:
            continue
        keep(src[end : m.start()])
        if m[0] in ("(*", "*)"):
            depth += 1 if m[0] == "(*" else -1
        else:
            keep(m[0])
        end = m.end()
    keep(src[end:])
    return "".join(out)


@functools.cache
def bare(src):
    """code(src) with the inside of every literal erased, line breaks kept:
    what is left of a line is OCaml that runs."""
    return LITERAL.sub(lambda m: '""' + "\n" * m[0].count("\n"), code(src))


def code_lines(src):
    """(line number, text) of each line of `src` that holds code."""
    lines = code(src or "").split("\n")
    return [(n + 1, l) for n, l in enumerate(lines) if l.strip()]


def shape(line):
    """`line` with names, constructors, literals and numbers erased, so code
    that differs only in those reads the same."""
    def erase(t):
        if t in KEYWORDS or not (t[0].isalnum() or t[0] == "_"):
            return t
        return "C" if t[0].isupper() else "0" if t[0].isdigit() else "x"
    return " ".join(erase(t) for t in TOKEN.findall(LITERAL.sub('"s"', line)))


def expected(rev, path):
    """The literals of `path` at `rev`, counted: the strings, chars, numbers
    and booleans a test expects."""
    return Counter(m[0] for m in EXPECTED.finditer(code(read(rev, path) or "")))


# Measures --------------------------------------------------------------------


def measured(path):
    """Whether `path` counts as "code", as "tests", or not at all."""
    if path.startswith("test/fixtures/"):
        return "tests"
    if path.endswith(OCAML):
        return "tests" if path.startswith("test/") else "code"
    return None


def counted(rev, path):
    """The lines of `path` at `rev` that count: code lines of OCaml, every
    non-blank line of a text fixture."""
    src = read(rev, path) or ""
    if path.endswith(OCAML):
        return len(code_lines(src))
    return 0 if "\0" in src else sum(1 for line in src.split("\n") if line.strip())


def tabular(run):
    """Whether the shapes in `run` repeat with a period of at most half its
    length, like the rows of a table."""
    return any(run[p:] == run[:-p] for p in range(1, len(run) // 2 + 1))


@functools.cache
def copied(rev, folder, window):
    """The lines of the .ml files under `folder` at `rev` that sit in a run of
    `window` code lines whose shape appears again under that folder, as
    {path: [line numbers]}. The rows of a table are not a copy."""
    runs, numbered = {}, {}
    for path in paths(rev, folder):
        if path.endswith(".ml"):
            numbered[path] = code_lines(read(rev, path))
            shapes = [shape(t) for _, t in numbered[path]]
            for k in range(len(shapes) - window + 1):
                run = shapes[k : k + window]
                if not tabular(run):
                    runs.setdefault("\n".join(run), []).append((path, k))
    lines = {}
    for starts in runs.values():
        for path, k in starts if len(starts) > 1 else []:
            lines.setdefault(path, set()).update(n for n, _ in numbered[path][k : k + window])
    return {p: sorted(ns) for p, ns in sorted(lines.items())}


def spans(lines):
    """`lines` as "path:first-last" spans."""
    out = []
    for path, ns in lines.items():
        first = prev = ns[0]
        for n in ns[1:] + [None]:
            if n is None or n > prev + 12:
                out.append(f"{path}:{first}-{prev}")
                first = n
            prev = n
    return out


ITEM = re.compile(r"\s*(val|external|type|module|include|exception|class|end|open)\b\s*(\(?[^\s:]*)")


def items(src):
    """(keyword, name, text) of each top-level item of an .mli, comments
    included, so a val's doc comment is in its text. An item starts on a line
    of code, never inside a comment."""
    lines, bare_lines = src.split("\n"), code(src).split("\n")
    starts = [i for i, l in enumerate(bare_lines) if ITEM.match(l)] + [len(lines)]
    return [(*ITEM.match(bare_lines[a]).groups(), "\n".join(lines[a:b])) for a, b in zip(starts, starts[1:])]


@functools.cache
def undocumented(rev):
    """The vals of lib/*.mli at `rev` without a doc comment of 4 or more
    words, as {"path: name"}."""
    out = set()
    for path in paths(rev, "lib/"):
        if path.endswith(".mli"):
            for kind, name, text in items(read(rev, path) or ""):
                doc = re.search(r"\(\*\*(.*?)\*\)", text, re.S)
                if kind in ("val", "external") and (not doc or len(doc[1].split()) < 4):
                    out.add(f"{path}: {name}")
    return frozenset(out)


def arguments(signature):
    """The argument types of a val signature, split at its top-level arrows."""
    parts, depth, start = [], 0, 0
    for i, ch in enumerate(signature):
        depth += (ch in "([{") - (ch in ")]}")
        if depth == 0 and signature.startswith("->", i):
            parts.append(signature[start:i])
            start = i + 2
    return [" ".join(p.split()) for p in parts]


@functools.cache
def lonely_labels(rev):
    """Labels in lib/*.mli on an argument whose type no other argument of the
    same val has, and that isn't a bool, as {"path: val ~label"}."""
    out = set()
    for path in paths(rev, "lib/"):
        if path.endswith(".mli"):
            for kind, name, text in items(code(read(rev, path) or "")):
                if kind == "val" and ":" in text:
                    # An .mli writes a labelled argument label:type, an optional one ?label:type.
                    args = [re.match(r"(\??)(?:([a-z_][\w']*):)?(.*)", a).groups() for a in arguments(text.split(":", 1)[1])]
                    types = Counter(t.strip() for _, _, t in args)
                    for optional, label, t in args:
                        if label and not optional and t.strip() != "bool" and types[t.strip()] == 1:
                            out.add(f"{path}: {name} ~{label}")
    return frozenset(out)


def definitions(src):
    """(name, code lines) of every let or and definition in `src`, nested
    ones included. A definition runs until the next code line indented no
    deeper than its first."""
    lines = [l for _, l in code_lines(src)]
    out = []
    for i, line in enumerate(lines):
        m = DEF.match(line)
        if m:
            depth = len(m[1])
            end = next((j for j in range(i + 1, len(lines)) if len(lines[j]) - len(lines[j].lstrip()) <= depth), len(lines))
            out.append((m[2] or "_", end - i))
    return out


@functools.cache
def long_definitions(rev, limit):
    """(path, name, code lines) of the definitions in lib/ and bin/ at `rev`
    that are longer than `limit`."""
    return [(p, name, n) for folder in ("lib/", "bin/") for p in paths(rev, folder) if p.endswith(".ml")
            for name, n in definitions(read(rev, p) or "") if n > limit]


def mutants(rev):
    """The mutants at `rev` (on disk when `rev` is None)."""
    out = []
    for p in paths(rev, "test/mutants/"):
        try:
            m = json.loads(read(rev, p))
            out.append({k: m[k] for k in ("file", "old", "new", "why")})
        except (ValueError, TypeError, KeyError) as e:
            sys.exit(f"{p} is not a mutant {{\"file\", \"old\", \"new\", \"why\"}}: {e!r}")
    return out


def key(mutant):
    return (mutant["file"], mutant["old"], mutant["new"])


# The checks on one commit. Each returns the problems it finds, each with the
# fix, in the words AGENTS.md uses for the rule.


class Commit:
    def __init__(self, sha, config):
        self.sha, self.config = sha, config
        self.parent = git("rev-parse", f"{sha}^").strip()
        self.message = git("log", "-1", "--format=%B", sha)
        m = SUBJECT.match(self.message)
        self.kind = m[1] if m else None
        self.budget = config["budgets"].get(self.kind) or config["budgets"]["other"]
        diff = git("diff", "--no-renames", "--name-only", "-z", self.parent, sha)
        self.changed = [p for p in diff.split("\0") if p]
        self.ocaml = [p for p in self.changed if p.endswith(OCAML)]
        self.net = Counter()
        for p in self.changed:
            if measured(p):
                self.net[measured(p)] += counted(sha, p) - counted(self.parent, p)
        self.copies = {}  # folder: (copied lines before, after, where after)
        for folder in ("lib/", "test/"):
            if any(p.startswith(folder) and p.endswith(".ml") for p in self.ocaml):
                before, after = (copied(r, folder, config["window"]) for r in (self.parent, sha))
                self.copies[folder] = (sum(map(len, before.values())), sum(map(len, after.values())), after)

    def code_changed(self, p):
        """Whether `p` changed beyond comments, blank lines and layout."""
        before, after = (TOKEN.findall(code(read(r, p) or "")) for r in (self.parent, self.sha))
        return before != after

    def changes_code(self):
        return any(measured(p) == "code" and self.code_changed(p) for p in self.ocaml)

    def guards(self, mutant):
        """Whether `mutant` sits on a line this commit changed."""
        src = read(self.sha, mutant["file"]) or ""
        if src.count(mutant["old"]) != 1:
            return False
        first = src[: src.index(mutant["old"])].count("\n") + 1
        lines = range(first, first + mutant["old"].count("\n") + 1)
        return not touched(self.parent, self.sha, mutant["file"]).isdisjoint(lines)


def check_message(c):
    if not c.kind:
        return [f"the subject is not `type(scope): Summary`: the type is one of {TYPES.replace('|', ', ')}, "
                "and the scope is lowercase letters, digits, `_`, `.` or `-`"]
    leak = LEAK.search(c.message)
    if leak:
        return [f"the message names a tool or carries a trailer (found `{leak[0].strip()}`): remove it, and turn off your editor's commit attribution"]
    if not any(l.strip() for l in c.message.split("\n")[1:]):
        return ["the message has no body: add a paragraph that says why the change is needed"]
    return []


def check_budget(c):
    out = []
    if c.net["code"] > c.budget["code"]:
        out.append(f"code {c.net['code']:+} over {c.budget['code']:+}: " + (
            "a refactor must not grow the code; make it shorter" if c.kind == "refactor"
            else "make it smaller, or split it into commits that each hold a whole behavior with its test and mutant"))
    if c.net["tests"] > c.budget["tests"]:
        out.append(f"tests {c.net['tests']:+} over {c.budget['tests']:+}: replace example cases with one property or one table")
    return out


def check_kind_honest(c):
    """docs, ci and chore change no code; test changes code only under test/; a
    feat or fix that changes code changes a test and adds a mutant on a line it
    changed; a refactor changes no test expectation."""
    if c.kind in ("docs", "ci", "chore", "test"):
        bad = [p for p in c.ocaml if not (c.kind == "test" and p.startswith("test/")) and c.code_changed(p)]
        return [f"a `{c.kind}` commit changes code in {', '.join(bad)}: move that change to a commit whose type allows it"] if bad else []
    if c.kind in ("feat", "fix") and c.changes_code():
        out = []
        if not any(p.startswith("test/fixtures/") or p.startswith("test/") and p.endswith(OCAML) and c.code_changed(p) for p in c.changed):
            out.append(f"a `{c.kind}` that changes code needs a test that fails without it: a property, a table row or a fixture")
        known = {key(m) for m in mutants(c.parent)}
        if not any(key(m) not in known and measured(m["file"]) == "code" and c.guards(m) for m in mutants(c.sha)):
            out.append(f"a `{c.kind}` that changes code adds a mutant, test/mutants/<behavior>.json, whose `old` is on a line this commit changed")
        return out
    if c.kind == "refactor":
        bad = [p for p in c.changed if p.startswith(("test/fixtures/", "test/oracle/"))]
        bad += [p for p in c.ocaml if p.startswith("test/") and expected(c.parent, p) != expected(c.sha, p)]
        return [f"a refactor changes test expectations in {', '.join(bad)}: a behavior change is a `fix` or `feat` commit of its own"] if bad else []
    return []


def check_files(c):
    """No new top-level path that gates.json doesn't list, and no symbolic link."""
    out = []
    new = {p.split("/")[0] for p in c.changed if read(c.parent, p) is None} - set(c.config["top_level"])
    if new:
        out.append(f"adds {', '.join(sorted(new))} at the top level: stage paths by name, never `git add -A`; "
                   "a new top-level path is the owner's call (gates.json)")
    raw = git("diff", "--raw", "--no-renames", "-z", c.parent, c.sha).split("\0")
    links = [path for meta, path in zip(raw[::2], raw[1::2]) if meta.split()[1] == "120000"]
    if links:
        out.append(f"{', '.join(links)} is a symbolic link: commit the file itself")
    return out


def check_headers(c):
    """A new module of lib/ has an .mli, and an .mli, or an .ml without one,
    opens with a doc comment."""
    out = [f"{p} has no .mli: write its interface, with a doc comment after every val" for p in c.ocaml
           if p.startswith("lib/") and p.endswith(".ml") and read(c.parent, p) is None
           and read(c.sha, p) is not None and read(c.sha, p + "i") is None]
    bad = [p for p in c.ocaml if read(c.sha, p) is not None and (p.endswith(".mli") or read(c.sha, p + "i") is None)
           and not read(c.sha, p).lstrip().startswith("(**")]
    return out + ([f"{', '.join(bad)} must open with a (** doc comment *) saying what the module is for"] if bad else [])


def check_docs(c):
    """A val added to lib/*.mli has a doc comment, and no val loses one."""
    if not any(p.startswith("lib/") and p.endswith(".mli") for p in c.ocaml):
        return []
    new = undocumented(c.sha) - undocumented(c.parent)
    return [f"no doc comment of 4 or more words after {', '.join(sorted(new))}: say what it returns, how it fails and why it exists"] if new else []


def check_labels(c):
    if not any(p.startswith("lib/") and p.endswith(".mli") for p in c.ocaml):
        return []
    new = lonely_labels(c.sha) - lonely_labels(c.parent)
    return [f"{', '.join(sorted(new))}: a label is only for arguments that share a type, or for a bool; make it positional"] if new else []


def check_pure(c):
    """Outside lib/runtime.ml and bin/main.ml, a line of code this commit
    changed does no I/O, keeps no mutable state, and raises only where a
    comment says why it cannot."""
    out = []
    for p in c.ocaml:
        src = read(c.sha, p)
        if src is None or measured(p) != "code" or not p.endswith(".ml") or p in ("lib/runtime.ml", "bin/main.ml"):
            continue
        raw, kept, lines = src.split("\n"), code(src).split("\n"), bare(src).split("\n")

        def says_why(k):  # "cannot raise: " and a reason of three or more words, in a comment of line k (0-based)
            return 0 <= k and bool(re.search(r"cannot raise:\s*\S+(\s+\S+){2}", raw[k])) and "cannot raise" not in kept[k]

        for n in sorted(touched(c.parent, c.sha, p)):
            line = lines[n - 1] if n <= len(lines) else ""
            impure, raises = IMPURE.search(line), RAISES.search(line)
            if impure:
                out.append(f"{p}:{n} uses `{impure[0]}`: I/O and mutable state live in lib/runtime.ml only; take a module argument instead")
            elif raises and not (says_why(n - 1) or says_why(n - 2)):
                out.append(f"{p}:{n} calls `{raises[0]}`: pure code returns an Error.t; if this call truly cannot raise, "
                           "say why in a `(* cannot raise: ... *)` comment on that line or the one above")
    return out


def check_runners(c):
    """No line of lib/ or bin/ this commit changed names another CWL runner."""
    hits = []
    for p in c.ocaml:
        src = read(c.sha, p)
        if src is not None and measured(p) == "code":
            lines = code(src).split("\n")
            hits += [f"{p}:{n}" for n in sorted(touched(c.parent, c.sha, p)) if n <= len(lines) and RUNNERS.search(lines[n - 1])]
    return [f"{', '.join(hits)} names another CWL runner: ccr runs CWL itself, and an unimplemented construct is `Unsupported`"] if hits else []


def check_format(c):
    """No commit switches ocamlformat off to pack lines."""
    bad = [p for p in c.ocaml if "ocamlformat" in bare(read(c.sha, p) or "") and "ocamlformat" not in bare(read(c.parent, p) or "")]
    return [f"{', '.join(bad)} switches ocamlformat off: let it format the code"] if bad else []


def check_todo(c):
    def todos(rev, p):
        return len(re.findall(r"(?i)\btodo\b|\b(FIXME|XXX)\b", read(rev, p) or ""))
    grew = [p for p in c.ocaml if todos(c.sha, p) > todos(c.parent, p)]
    return [f"a TODO in {', '.join(grew)}: do it now, or open a GitHub issue with the repro instead"] if grew else []


def check_function_length(c):
    """The lines by which definitions in lib/ and bin/ exceed the limit,
    summed, never rise: a definition already over it shrinks before it grows,
    and a rename or a move keeps the sum."""
    if not c.changes_code():
        return []
    limit = c.config["max_function"]
    before, after = (sum(n - limit for _, _, n in long_definitions(r, limit)) for r in (c.parent, c.sha))
    if after <= before:
        return []
    names = [f"`{name}` ({p}, {n} code lines)" for p, name, n in long_definitions(c.sha, limit) if p in c.changed]
    return [f"code lines over the {limit}-line limit rise {before}→{after} in {', '.join(names)}: "
            "split by decision; a definition that was already over the limit shrinks in a `refactor` before it grows"]


def check_copies(c):
    return [f"copied lines in {folder} rise {before}→{after} ({', '.join(spans(where))}): make the copies one function, one table or one property"
            for folder, (before, after, where) in c.copies.items() if after > before]


def check_mutants_kept(c):
    """No mutant disappears or changes unless the line it guards moved: its
    file changed in this commit."""
    now = {key(m) for m in mutants(c.sha)}
    lost = [m for m in mutants(c.parent) if key(m) not in now and m["file"] not in c.changed]
    out = [f"the mutant for \"{m['why']}\" is gone, but {m['file']} did not change: keep it" for m in lost]
    if len(mutants(c.sha)) < len(mutants(c.parent)):
        out.append(f"mutants {len(mutants(c.parent))}→{len(mutants(c.sha))}: re-anchor a mutant whose line moved; never delete one")
    return out


CHECKS = [check_message, check_budget, check_kind_honest, check_files, check_headers, check_docs,
          check_labels, check_pure, check_runners, check_format, check_todo, check_function_length,
          check_copies, check_mutants_kept]


def check_commits(shas, config):
    rows = ["| commit | type | code | tests | copied lib / test | result |", "|---|---|---|---|---|---|"]
    failed, total = 0, Counter()
    for sha in shas:
        c = Commit(sha, config)
        problems = [p for check in CHECKS for p in check(c)]
        failed += bool(problems)
        total.update(c.net)  # update keeps the negative sums that += would drop
        dups = " / ".join(f"{c.copies[f][0]}→{c.copies[f][1]}" if f in c.copies else "-" for f in ("lib/", "test/"))
        rows.append(f"| {sha[:7]} | {c.kind or '?'} | {c.net['code']:+} / {c.budget['code']:+} | "
                    f"{c.net['tests']:+} / {c.budget['tests']:+} | {dups} | {'<br>'.join(problems) or 'ok'} |")
    if len(shas) > 1:
        rows.append(f"| all {len(shas)} | | {total['code']:+} | {total['tests']:+} | | |")
    return rows, failed


def config_at(base):
    return json.loads(read(base, "gates.json") or Path("gates.json").read_text())


def check_base(base, head):
    merges = git("rev-list", "--merges", f"{base}..{head}").split()
    rows, failed = check_commits(git("rev-list", "--reverse", "--no-merges", f"{base}..{head}").split(), config_at(base))
    rows += [f"| {sha[:7]} | merge | | | | a merge commit: rebase onto `{base}` instead (linear history) |" for sha in merges]
    report("Size and shape", rows + (["", 'Fix each problem in the commit that has it. AGENTS.md, "Size and shape", explains every limit.'] if failed or merges else []))
    return failed + len(merges)


def check_worktree(message):
    """Check the working tree as one commit on top of HEAD, made in a
    temporary index: HEAD, the index and the files stay as they are."""
    config = config_at("HEAD")
    with tempfile.TemporaryDirectory() as tmp:
        env = dict(os.environ, GIT_INDEX_FILE=f"{tmp}/index")
        git("read-tree", "HEAD", env=env)
        git("add", "-A", "--", *[p for p in config["top_level"] if Path(p).exists()], env=env)
        tree = git("write-tree", env=env).strip()
    sha = git("commit-tree", tree, "-p", "HEAD", "-m", message).strip()
    rows, failed = check_commits([sha], config)
    report("Size and shape (working tree)", rows)
    return failed


def dune(*args, cwd=None, env=None):
    """The exit code of `dune args` run in `cwd`, or None after 10 minutes."""
    try:
        return subprocess.run(["dune", *args], cwd=cwd, env=env, capture_output=True, timeout=600).returncode
    except FileNotFoundError:
        sys.exit("dune not found: run this under `opam exec --`")
    except subprocess.TimeoutExpired:
        return None


def tests(tree):
    """How the code in `tree` fares: "broken" when it does not build, else
    whether its quick tests "pass" or "fail", or "timeout"."""
    built = dune("build", cwd=tree)
    quick = dict(os.environ, ALCOTEST_QUICK_TESTS="1", ALCOTEST_BAIL="1")
    tested = dune("runtest", "--force", cwd=tree, env=quick) if built == 0 else built
    return {None: "timeout", 0: "pass"}.get(tested, "fail" if built == 0 else "broken")


def test_names(tree):
    """The test cases of the build in `tree`, as "suite: name"."""
    exe = Path(tree, "_build/default/test/test_cwl.exe")
    listed = subprocess.run([exe, "list"], cwd=tree, capture_output=True, text=True).stdout if exe.exists() else ""
    return {f"{m[1]}: {m[2]}" for m in re.finditer(r"^(\S+) +\d+ +(.*)\.$", listed, re.M)}


def check_each(base, head):
    """In a scratch worktree, check every commit of the range: it builds and is
    formatted; every test case it removes is named in its message; and a feat
    or fix that changes code has tests that fail with its parent's code."""
    config = config_at(base)
    rows = ["| commit | builds and is formatted | test cases kept | tests fail without its code |", "|---|---|---|---|"]
    failed = 0
    with tempfile.TemporaryDirectory() as tmp:
        tree = f"{tmp}/tree"
        git("worktree", "add", "-q", "--detach", tree, base)
        try:
            names = test_names(tree) if dune("build", cwd=tree) == 0 else set()
            for sha in git("rev-list", "--reverse", "--no-merges", f"{base}..{head}").split():
                c = Commit(sha, config)
                git("-C", tree, "checkout", "-q", "-f", "--detach", sha)
                git("-C", tree, "clean", "-q", "-f", "-d")  # what the last commit's check left behind
                built = dune("build", cwd=tree) == 0 and dune("build", "@fmt", cwd=tree) == 0
                now = test_names(tree) if built else names
                lost = sorted(n for n in names - now if n not in c.message)
                names, bites = now, "-"
                if c.kind in ("feat", "fix") and c.changes_code():
                    for p in c.changed:  # the commit's tests over its parent's code
                        if measured(p) == "code":
                            old = read(c.parent, p)
                            Path(tree, p).unlink(missing_ok=True) if old is None else Path(tree, p).write_text(old)
                    bites = "ok" if tests(tree) != "pass" else "the quick tests pass with the parent's code: add one that fails without this commit"
                cells = ["ok" if built else "does not build or is not formatted: fix it in this commit",
                         f"gone: {', '.join(lost)}; name each in the message, with what covers it now" if lost else "ok", bites]
                failed += any(cell not in ("ok", "-") for cell in cells)
                rows.append(f"| {sha[:7]} | {' | '.join(cells)} |")
        finally:
            git("worktree", "remove", "--force", tree)
    report("Every commit", rows)
    return failed


def check_mutants():
    """Copy the working tree to a scratch directory, check that it builds and
    passes the quick tests, then apply each mutant alone: it must build and
    fail them."""
    rows, failed = ["| mutant | result |", "|---|---|"], 0
    with tempfile.TemporaryDirectory() as tmp:
        shutil.copytree(".", tmp, dirs_exist_ok=True, ignore=shutil.ignore_patterns(".git", "_build", "_opam", "vendor"))
        start = tests(tmp)
        if start != "pass":
            report("Mutants", rows + [f"| (none) | with no mutant the code is {start}: fix that first |"])
            return 1
        code_files = paths(None, "lib/") + paths(None, "bin/")
        for m in mutants(None):
            src = read(None, m["file"]) or ""
            if m["file"] not in code_files:
                result = f"{m['file']} is not a file in lib/ or bin/: point `file` at the code that holds the behavior"
            elif src.count(m["old"]) != 1:
                result = f"its `old` is found {src.count(m['old'])} times in {m['file']}: anchor it on one line (ocamlformat may have re-wrapped it)"
            else:
                Path(tmp, m["file"]).write_text(src.replace(m["old"], m["new"]))
                outcome = tests(tmp)
                Path(tmp, m["file"]).write_text(src)
                result = {"fail": "ok", "broken": "the mutated code does not build: choose a mutation that compiles",
                          "timeout": "the build or the tests time out",
                          "pass": "survives: no quick test notices this behavior break; add or fix the test"}[outcome]
            failed += result != "ok"
            rows.append(f"| {m['file']}: {m['why']} | {result} |")
    report("Mutants", rows)
    return failed


def check_pr(event_path):
    """The PR's title, body and branches, from the GitHub event at `event_path`."""
    event = json.loads(Path(event_path).read_text())
    pr, problems = event["pull_request"], []
    title, raw = pr["title"], pr.get("body") or ""
    body = re.sub(r"<!--.*?-->", "", raw, flags=re.S)  # the template's hints are not content
    if not SUBJECT.match(title):
        problems.append("the title is not `type(scope): Summary`: use the subject of the PR's main commit")
    if LEAK.search(title + "\n" + raw):
        problems.append("the title or body names a tool or carries an attribution line: remove it")
    if NARRATION.search(body):
        problems.append("the body narrates local test runs: CI shows them, delete that")
    if re.search(r"#[NMK]\b|\bK commits\b", body):
        problems.append("the Stack line still has the template's placeholders: fill it in, or delete it when the PR is not stacked")
    diff = subprocess.run(["git", "diff", "--name-only", f"origin/{pr['base']['ref']}...{pr['head']['sha']}"], capture_output=True, text=True)
    ocaml = diff.returncode != 0 or any(p.endswith(OCAML) for p in diff.stdout.split())  # unknown counts as OCaml
    wanted = SECTIONS if ocaml else SECTIONS[:2]
    problems += [f"the body has no {s} section: fill in .github/pull_request_template.md" for s in wanted if s not in body]
    found = sorted((body.index(s), s) for s in SECTIONS if s in body)
    for (start, s), (end, _) in zip(found, found[1:] + [(len(body), None)]):
        if s in wanted and not re.search(r"\w", body[start + len(s) : end]):
            problems.append(f"nothing is written under {s}")
    if pr["base"]["ref"] != event["repository"]["default_branch"]:
        problems.append(f"the PR targets `{pr['base']['ref']}`: every PR targets `{event['repository']['default_branch']}`, and the stack is the branch ancestry")
    if not BRANCH.fullmatch(pr["head"]["ref"]):
        problems.append(f"the branch `{pr['head']['ref']}` is not `<type>/<short-slug>`, with a commit type and a slug of lowercase letters, digits, `.`, `_` or `-`")
    report("PR text", ["| check | result |", "|---|---|"] + [f"| PR #{pr['number']} | {p} |" for p in problems or ["ok"]])
    return len(problems)


def report(title, rows):
    text = f"### {title}\n\n" + "\n".join(rows) + "\n"
    print(text)
    if os.environ.get("GITHUB_STEP_SUMMARY"):
        with open(os.environ["GITHUB_STEP_SUMMARY"], "a") as summary:
            summary.write(text)


def main():
    signal.signal(signal.SIGTERM, lambda *_: sys.exit(143))  # a timeout still removes the scratch copies
    args = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    args.add_argument("--base", metavar="REF")
    args.add_argument("--head", metavar="REV", default="HEAD")
    args.add_argument("--worktree", action="store_true")
    args.add_argument("-m", "--message", help="the full message the working tree would be committed with")
    args.add_argument("--each", action="store_true")
    args.add_argument("--mutants", action="store_true")
    args.add_argument("--dups", action="store_true")
    args.add_argument("--pr", metavar="EVENT_JSON")
    a = args.parse_args()
    if not (a.base or a.worktree or a.mutants or a.dups or a.pr):
        args.print_help()
        sys.exit(2)
    if a.worktree and not a.message:
        args.error("--worktree needs -m MESSAGE: the commit's type decides its budget")
    if a.each and not a.base:
        args.error("--each needs --base REF")
    a.pr = a.pr and os.path.abspath(a.pr)
    os.chdir(git("rev-parse", "--show-toplevel").strip())  # every path below is relative to the root
    if a.dups:
        window = config_at("HEAD")["window"]
        print("\n".join(s for folder in ("lib/", "test/") for s in spans(copied(None, folder, window))) or "no copied blocks")
    failed = 0
    if a.pr:
        failed += check_pr(a.pr)
    if a.worktree:
        failed += check_worktree(a.message)
    if a.base and a.each:
        failed += check_each(a.base, a.head)
    elif a.base:
        failed += check_base(a.base, a.head)
    if a.mutants:
        failed += check_mutants()
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
