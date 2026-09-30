# caml-cwl-runner

A CWL v1.2.1 runner in OCaml 5.5. Binary name: `ccr`.

`ccr` executes a `CommandLineTool`, locally or under Docker (no JavaScript yet), and
prints the CWL output object as JSON on stdout. Exit 33 means an
unimplemented feature was required.

## Setup

```bash
git submodule update --init
opam switch create . ocaml-base-compiler.5.5.0 --no-install
eval $(opam env)
opam install . --deps-only --with-test --with-dev-setup -y
```

## Build / test

```bash
dune build
dune runtest
dune exec -- ccr --version
dune fmt
```

## Conformance

```bash
pip install cwltest cwltool
scripts/conformance.py                 # v1.2 suite, ratcheted in conformance/
scripts/conformance.py --suite oracle  # cwltool-verified probes in test/oracle
```

Source of truth: the CWL v1.2 and Schema Salad specs, then the conformance
tests, then cwltool.

Architecture: [design_docs/runner.md](design_docs/runner.md).
