#!/usr/bin/env python3
"""Fill expected outputs in test/oracle/tests.yaml by running cwltool.

For each entry (or only the ids given), run cwltool on its tool and job and
write the normalized output object back as `output`. If cwltool fails, the
entry becomes `should_fail: true`. Nobody types an expected value by hand.

Normalization keeps what cwltest compares and drops what depends on the
machine: File/Directory `location` becomes the basename, `path` is removed,
and `class`, `checksum`, `size`, `basename`, `secondaryFiles`, and `listing`
are kept.
"""

import argparse
import json
import os
import subprocess
import sys
import tempfile

import yaml

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TESTS = os.path.join(ROOT, "test/oracle/tests.yaml")
HEADER = "# Expected outputs are written by scripts/oracle.py from cwltool. Do not edit them by hand.\n"


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


def record(entry, base):
    cmd = ["cwltool", "--quiet"]
    with tempfile.TemporaryDirectory(prefix="oracle-") as out:
        cmd += ["--outdir", out, os.path.join(base, entry["tool"])]
        if entry.get("job"):
            cmd.append(os.path.join(base, entry["job"]))
        proc = subprocess.run(cmd, capture_output=True, text=True, cwd=base)
    entry.pop("output", None)
    entry.pop("should_fail", None)
    if proc.returncode != 0:
        entry["should_fail"] = True
        return f"should_fail (cwltool exit {proc.returncode})"
    entry["output"] = normalize(json.loads(proc.stdout))
    return "output"


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("ids", nargs="*", help="entries to record (default: all)")
    args = ap.parse_args()

    with open(TESTS) as f:
        tests = yaml.safe_load(f)
    base = os.path.dirname(TESTS)
    unknown = set(args.ids) - {t["id"] for t in tests}
    if unknown:
        sys.exit(f"unknown ids: {' '.join(sorted(unknown))}")
    for entry in tests:
        if not args.ids or entry["id"] in args.ids:
            print(f"{entry['id']}: {record(entry, base)}")
    with open(TESTS, "w") as f:
        f.write(HEADER)
        yaml.safe_dump(tests, f, sort_keys=False, width=100)
    return 0


if __name__ == "__main__":
    sys.exit(main())
