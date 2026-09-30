#!/usr/bin/env python3
"""Check the cases in test/oracle/tests.yaml and fill their expected outputs.

Every case names the authority for its expected output:

- `ccr:spec` and `ccr:quote`: a vendored spec file and the sentences in it
  that pin the output. cwltool is a second opinion. When it disagrees, the
  case is reported as a divergence and left as written. A new case with no
  output yet is seeded from cwltool and must be checked against the quote
  before it is committed.
- `ccr:unspecified`: what the spec leaves open. The output is cwltool's,
  recorded for interoperability, and a rerun replaces it.

With --check, only the authorities are checked, without running cwltool:
each case has exactly one, and each quote appears in its spec file
(markdown links, code marks, emphasis, and line breaks are ignored).

A recorded output keeps what cwltest compares and drops what depends on the
machine: File/Directory `location` becomes the basename, `path` is removed,
and `class`, `checksum`, `size`, `basename`, `secondaryFiles`, and `listing`
are kept.
"""

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile

from cwltest.compare import CompareFail, compare
from ruamel.yaml import YAML

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TESTS = os.path.join(ROOT, "test/oracle/tests.yaml")
SPEC, QUOTE, UNSPECIFIED = "ccr:spec", "ccr:quote", "ccr:unspecified"


def yaml():
    """Round-trip YAML, so comments and folded quotes survive a rewrite.

    The width is past any line in the file: quotes keep their own folds, and
    plain scalars are never folded.
    """
    y = YAML()
    y.indent(mapping=2, sequence=2, offset=0)
    y.width = 4096
    return y


def prose(text):
    """Text as a reader sees it: no markdown links, code marks, or line breaks."""
    text = re.sub(r"\[([^\]]*)\]\([^)]*\)", r"\1", text)
    return " ".join(text.replace("`", "").replace("**", "").split())


def problems(case):
    """What is wrong with the case's authority; empty when nothing is."""
    if (SPEC in case) == (UNSPECIFIED in case):
        return [f"needs exactly one of {SPEC} and {UNSPECIFIED}"]
    if UNSPECIFIED in case:
        return []
    path = os.path.join(ROOT, case[SPEC])
    if not os.path.isfile(path):
        return [f"{case[SPEC]} is not a file"]
    quotes = case.get(QUOTE) or []
    quotes = [quotes] if isinstance(quotes, str) else quotes
    if not quotes:
        return [f"{SPEC} needs a {QUOTE}"]
    with open(path) as f:
        text = prose(f.read())
    return [f"not in {case[SPEC]}: {q}" for q in quotes if prose(q) not in text]


def normalize(v):
    if isinstance(v, list):
        return [normalize(x) for x in v]
    if isinstance(v, dict):
        if v.get("class") in ("File", "Directory"):
            keep = {"class", "checksum", "size", "basename", "secondaryFiles", "listing"}
            out = {k: normalize(x) for k, x in v.items() if k in keep}
            loc = v.get("location") or v.get("path")
            if loc:
                out["location"] = os.path.basename(loc.rstrip("/"))
            return out
        return {k: normalize(x) for k, x in v.items()}
    return v


def record(case, got, code):
    case.pop("output", None)
    case.pop("should_fail", None)
    if got is None:
        case["should_fail"] = True
        return f"recorded should_fail (cwltool exit {code})"
    case["output"] = normalize(got)
    return "recorded output"


def divergence(case, got):
    """Why cwltool's result contradicts the case, or None if it agrees."""
    if case.get("should_fail"):
        return None if got is None else "cwltool succeeded"
    if got is None:
        return "cwltool failed"
    try:
        compare(case["output"], got)
    except CompareFail as e:
        return str(e).strip().splitlines()[-1]
    return None


def consult(case, base):
    """Run cwltool on the case; return a status line and whether cwltool agrees."""
    cmd = ["cwltool", "--quiet"]
    with tempfile.TemporaryDirectory(prefix="oracle-") as out:
        cmd += ["--outdir", out, os.path.join(base, case["tool"])]
        if case.get("job"):
            cmd.append(os.path.join(base, case["job"]))
        proc = subprocess.run(cmd, capture_output=True, text=True, cwd=base)
        got = json.loads(proc.stdout) if proc.returncode == 0 else None
        if UNSPECIFIED in case:
            return record(case, got, proc.returncode), True
        if "output" not in case and not case.get("should_fail"):
            return record(case, got, proc.returncode) + "; seeded, check it against the quote", True
        # compare reads the output files, so this runs before out is removed.
        why = divergence(case, got)
    if why is None:
        return "cwltool agrees", True
    return f"CWLTOOL DIVERGES from the spec case: {why}", False


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--check", action="store_true",
                    help="only check that each case names its authority; do not run cwltool")
    ap.add_argument("ids", nargs="*", help="cases to run (default: all)")
    args = ap.parse_args()

    y = yaml()
    with open(TESTS) as f:
        doc = y.load(f)
    cases = doc["$graph"]
    unknown = set(args.ids) - {c["id"] for c in cases}
    if unknown:
        sys.exit(f"unknown ids: {' '.join(sorted(unknown))}")
    cases = [c for c in cases if not args.ids or c["id"] in args.ids]

    bad = [f"{c['id']}: {p}" for c in cases for p in problems(c)]
    print("\n".join(bad) if bad else f"checked the authority of {len(cases)} case(s)")
    if bad or args.check:
        return 1 if bad else 0

    base = os.path.dirname(TESTS)
    diverged = 0
    for case in cases:
        status, agrees = consult(case, base)
        diverged += not agrees
        print(f"{case['id']}: {status}")
    with open(TESTS, "w") as f:
        y.dump(doc, f)
    return 1 if diverged else 0


if __name__ == "__main__":
    sys.exit(main())
