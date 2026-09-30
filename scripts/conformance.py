#!/usr/bin/env python3
"""Run a cwltest suite against ccr and ratchet the passing test ids.

Check (default): exit 1 if a test listed as passing no longer passes.
--update: rewrite the list to the tests that pass now.

Only the summary is printed. Full logs are under _build/conformance/:
<suite>.log (cwltest output), <suite>.xml (JUnit), and <suite>.status
(one "id<TAB>status<TAB>tags" line per test run).
"""

import argparse
import os
import subprocess
import sys
import xml.etree.ElementTree as ET
from collections import Counter

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SUITES = {
    "v1.2": ("vendor/cwl-v1.2/conformance_tests.yaml", "conformance/v1.2.passing"),
    "oracle": ("test/oracle/tests.yaml", "conformance/oracle.passing"),
}
KEY_TAGS = ["required", "command_line_tool", "workflow", "inline_javascript", "docker"]


def read_list(path):
    if not os.path.exists(path):
        return set()
    with open(path) as f:
        return {line.strip() for line in f if line.strip() and not line.startswith("#")}


def statuses(junit):
    """Map test id to (status, tags) from cwltest's JUnit report."""
    out = {}
    for case in ET.parse(junit).getroot().iter("testcase"):
        tid = case.get("file")
        tags = [t.strip() for t in (case.get("class") or "").split(",") if t.strip()]
        if case.find("failure") is not None:
            status = "fail"
        elif case.find("skipped") is not None:
            status = "unsupported"
        else:
            status = "pass"
        out[tid] = (status, tags)
    return out


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--suite", choices=sorted(SUITES), default="v1.2")
    ap.add_argument("--tags", help="comma-separated cwltest tags to run")
    ap.add_argument("--exclude-tags", help="comma-separated cwltest tags to skip")
    ap.add_argument("-j", type=int, default=os.cpu_count() or 2)
    ap.add_argument("--timeout", type=int, default=120, help="seconds per test")
    ap.add_argument("--tool", help="runner to test (default: the dune-built ccr)")
    ap.add_argument("--update", action="store_true", help="rewrite the passing list")
    args = ap.parse_args()

    os.chdir(ROOT)
    test_file, list_file = SUITES[args.suite]
    tool = args.tool
    if tool is None:
        subprocess.run(["dune", "build", "./bin/main.exe"], check=True)
        tool = os.path.join(ROOT, "_build/default/bin/main.exe")

    work = os.path.join(ROOT, "_build/conformance")
    os.makedirs(work, exist_ok=True)
    junit = os.path.join(work, f"{args.suite}.xml")
    log = os.path.join(work, f"{args.suite}.log")
    cmd = ["cwltest", "--test", test_file, "--tool", tool, "-j", str(args.j),
           "--timeout", str(args.timeout), "--junit-xml", junit]
    if args.tags:
        cmd += ["--tags", args.tags]
    if args.exclude_tags:
        cmd += ["--exclude-tags", args.exclude_tags]
    with open(log, "w") as f:
        subprocess.run(cmd, stdout=f, stderr=subprocess.STDOUT)

    got = statuses(junit)
    with open(os.path.join(work, f"{args.suite}.status"), "w") as f:
        for tid in sorted(got):
            status, tags = got[tid]
            f.write(f"{tid}\t{status}\t{','.join(tags)}\n")

    counts = Counter(s for s, _ in got.values())
    print(f"{args.suite}: {len(got)} run | {counts['pass']} pass | {counts['fail']} fail | "
          f"{counts['unsupported']} unsupported")
    for tag in KEY_TAGS:
        run = [s for s, tags in got.values() if tag in tags]
        if run:
            print(f"  {tag}: {sum(s == 'pass' for s in run)}/{len(run)} pass")

    passing_now = {tid for tid, (s, _) in got.items() if s == "pass"}
    listed = read_list(list_file)
    regressed = sorted(tid for tid in listed if tid in got and tid not in passing_now)
    new = sorted(passing_now - listed)

    if args.update:
        kept = {tid for tid in listed if tid not in got}  # outside this run's tags
        os.makedirs(os.path.dirname(list_file), exist_ok=True)
        with open(list_file, "w") as f:
            f.write(f"# Passing ids for {test_file}. Written by scripts/conformance.py --update.\n")
            for tid in sorted(kept | passing_now):
                f.write(tid + "\n")
        print(f"wrote {list_file}: {len(kept | passing_now)} ids")
        return 0

    if new:
        print(f"newly passing ({len(new)}): {' '.join(new)}")
        print("  record them with --update")
    if regressed:
        print(f"REGRESSED ({len(regressed)}): {' '.join(regressed)}")
        print(f"  details: {log}")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
