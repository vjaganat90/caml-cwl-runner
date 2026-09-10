# caml-cwl-runner

A CWL v1.2.1 runner in OCaml 5.5. Binary name: `ccr`.

`ccr` executes a local `CommandLineTool` (no Docker, no JavaScript) and
prints the CWL output object as JSON on stdout. Exit 33 means an
unimplemented feature was required.

## Setup

```bash
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

Source of truth: CWL v1.2 spec, then conformance tests, then cwltool only
for a disputed corner.

Architecture: [design_docs/runner.md](design_docs/runner.md).
