#!/usr/bin/env python3
"""Run a cwltest suite against ccr and report what passes.

With --baseline REF, ccr is also built at REF (in a worktree under .git/) and
the same suite runs against both binaries. Exit 1 if a test that passes at REF
does not pass now. CI passes the PR's base branch, so nothing is recorded by
hand and both runs happen on the same machine.

Only the summary is printed. Full logs are under _build/conformance/:
<suite>[-baseline].log (cwltest output), .xml (JUnit), and .status
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
    "v1.2": "vendor/cwl-v1.2/conformance_tests.yaml",
    "oracle": "test/oracle/tests.yaml",
}
KEY_TAGS = ["required", "command_line_tool", "workflow", "inline_javascript", "docker"]
WORK = os.path.join(ROOT, "_build/conformance")


def git(*args, cwd=ROOT):
    return subprocess.run(["git", *args], cwd=cwd, check=True,
                          capture_output=True, text=True).stdout.strip()


def build(root):
    """Build ccr in the dune project at root and return the binary."""
    subprocess.run(["dune", "build", "--root", ".", "./bin/main.exe"], cwd=root, check=True)
    return os.path.join(root, "_build/default/bin/main.exe")


def baseline_worktree():
    return os.path.join(git("rev-parse", "--path-format=absolute", "--git-common-dir"),
                        "conformance-baseline")


def drop_baseline():
    subprocess.run(["git", "worktree", "remove", "--force", baseline_worktree()],
                   cwd=ROOT, capture_output=True)


def build_at(ref):
    """Check REF out in a scratch worktree inside the git dir and build ccr there."""
    sha = git("rev-parse", "--verify", f"{ref}^{{commit}}")
    drop_baseline()
    git("worktree", "add", "-q", "--detach", baseline_worktree(), sha)
    return build(baseline_worktree())


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


def run_suite(args, tool, name):
    """Run cwltest with tool; write name.{log,xml,status}; return statuses."""
    junit, log = os.path.join(WORK, f"{name}.xml"), os.path.join(WORK, f"{name}.log")
    cmd = ["cwltest", "--test", SUITES[args.suite], "--tool", tool, "-j", str(args.j),
           "--timeout", str(args.timeout), "--junit-xml", junit]
    if args.tags:
        cmd += ["--tags", args.tags]
    if args.exclude_tags:
        cmd += ["--exclude-tags", args.exclude_tags]
    with open(log, "w") as f:
        subprocess.run(cmd, cwd=ROOT, stdout=f, stderr=subprocess.STDOUT)
    got = statuses(junit)
    with open(os.path.join(WORK, f"{name}.status"), "w") as f:
        for tid in sorted(got):
            status, tags = got[tid]
            f.write(f"{tid}\t{status}\t{','.join(tags)}\n")
    return got


def summary(label, got):
    counts = Counter(s for s, _ in got.values())
    print(f"{label}: {len(got)} run | {counts['pass']} pass | {counts['fail']} fail | "
          f"{counts['unsupported']} unsupported")
    for tag in KEY_TAGS:
        run = [s for s, tags in got.values() if tag in tags]
        if run:
            print(f"  {tag}: {sum(s == 'pass' for s in run)}/{len(run)} pass")


def passing(got):
    return {tid for tid, (s, _) in got.items() if s == "pass"}


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--suite", choices=sorted(SUITES), default="v1.2")
    ap.add_argument("--tags", help="comma-separated cwltest tags to run")
    ap.add_argument("--exclude-tags", help="comma-separated cwltest tags to skip")
    ap.add_argument("-j", type=int, default=os.cpu_count() or 2)
    ap.add_argument("--timeout", type=int, default=120, help="seconds per test")
    ap.add_argument("--tool", help="runner to test (default: ccr built from this checkout)")
    ap.add_argument("--baseline", metavar="REF",
                    help="also test ccr built at REF; fail if a test passing there does not pass now")
    args = ap.parse_args()

    os.makedirs(WORK, exist_ok=True)
    now = run_suite(args, args.tool or build(ROOT), args.suite)
    summary(args.suite, now)
    if not args.baseline:
        return 0

    try:
        before = run_suite(args, build_at(args.baseline), f"{args.suite}-baseline")
    finally:
        drop_baseline()
    summary(f"{args.suite} at {args.baseline}", before)
    new = sorted(passing(now) - passing(before))
    regressed = sorted(passing(before) - passing(now))
    if new:
        print(f"newly passing ({len(new)}): {' '.join(new)}")
    if regressed:
        print(f"REGRESSED ({len(regressed)}): {' '.join(regressed)}")
        print(f"  details: {os.path.join(WORK, args.suite + '.log')}")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
