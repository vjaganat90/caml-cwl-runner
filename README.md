# caml-cwl-runner

A CWL v1.2.1 runner in OCaml 5.5. Binary name: `ccr`.

`ccr` executes a `CommandLineTool`, locally or under Docker, and prints the
CWL output object as JSON on stdout. Not built yet: Workflow execution and
JavaScript expressions. Anything ccr reads but doesn't implement is
reported, never silently dropped.

## Setup

```bash
git submodule update --init
opam switch create . ocaml-base-compiler.5.5.0 --no-install
eval $(opam env)
opam install . --deps-only --with-test -y
opam install ocamlformat.0.29.0 -y        # the version .ocamlformat pins
pip install -r scripts/requirements.txt   # pinned cwltest and cwltool
```

## Use

```
ccr [--outdir DIR] [--quiet] [--rm-tmpdir | --leave-tmpdir] PROCESS [JOB]
```

`PROCESS` is a CWL document; `JOB` is its input object (none means `{}`).
Outputs go to `DIR`, or to a new temporary directory that is left in place.
Diagnostics go to stderr (`--quiet` hides them). Exit 0 on success, 33 when
the process needs a CWL feature ccr doesn't implement, 1 for any other
failure.

```bash
$ dune exec -- ccr test/fixtures/echo-stdout.cwl test/fixtures/echo-job.json
{"example_out":{"class":"File","location":"file:///…/out.txt","basename":"out.txt",…,"size":6}}
```

## Build and test

```bash
dune build
dune runtest
dune fmt
scripts/conformance.py --baseline origin/main             # v1.2 suite vs main
scripts/conformance.py --suite oracle --baseline origin/main  # spec-quoted cases
```

## More

- Changing the code: read [AGENTS.md](AGENTS.md) first. It holds every rule,
  and what "done" means.
- How the engine works: [design_docs/runner.md](design_docs/runner.md).
