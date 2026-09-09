# caml-cwl-runner

A CWL v1.2.1 runner in OCaml 5.5. Binary name: `ccr`.

Slice 1 loads a `CommandLineTool` and a job and builds argv. It does not
execute processes. `ccr tool.cwl job.json` prints diagnostics and exits 33
(unimplemented feature).

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
for a disputed corner. This is not a cwltool port.
