# caml-cwl-runner

CWL v1.2.1 runner in OCaml 5.5. Binary: `ccr`.

Source of truth, in order: the CWL v1.2 spec (CommandLineTool, Workflow,
Process, `invocation.md`) and the Schema Salad spec, then the v1.2
conformance tests in `vendor/cwl-v1.2`. cwltool decides only what the spec
leaves open. Not a cwltool port: match its behavior where the spec is
silent, don't copy its code.

CLI: `ccr [tool] [job]`. On success, print the output object as JSON on
stdout. Exit 33 means an unimplemented feature was required. Any other
failure is exit 1.

## Modules

`.mli` is the contract. ADTs are defined once in private `Data` and
`include`d into the public modules. Effectful holes are module arguments
`(module E : ENGINE)`, `(module FS)`, `(module RUNTIME)`,
`(module FILE)`. Stdlib and `Result.t`. No objects, `Obj`, refs, or
`Hashtbl` unless a Runtime body needs them.

`Untyped_tree` is the nested YAML/JSON value, before CWL types. It is the
only document reader. `Document.of_tree` reads `class` and returns
`Command_line_tool.t` or `Workflow.t`. A job file is an `Untyped_tree`
turned into `Type.object_`, not a `Document`.

`Schema` holds the fields and parsers shared by both process classes:
inputs, outputs, requirements, types, bindings. `Command_line_tool` adds
`baseCommand`, arguments, stdio, and success codes. `Workflow` is its own
record and calls `Schema` for the shared fields. Graph execution is not
implemented; `Document.Workflow` is `Unsupported`.

I/O is only in `Untyped_tree` (load) and `Runtime` (filesystem and spawn).
Eio is the Runtime body and the CLI scheduler (`Eio_main.run`). No Lwt.
`Glob` is pure given `(module FS)`. A runnable `DockerRequirement` is one
image source (`dockerPull`, `dockerImageId`, `dockerLoad`, `dockerImport`,
or `dockerFile`) and selects `Runtime.docker`. `dockerOutputDirectory`, when
set, is a canonical absolute container path: `runtime.outdir` and the
workdir are that path, and the host outdir is bind-mounted there. The
volume source is `realpath` of the host directory. A staged file's
`location` is the container path. Absolute output paths under it are read
back on the host. A repeated `DockerRequirement` is a schema error. In
hints the class is a diagnostic and execution stays local.

An unimplemented CWL construct is a `diagnostic`. In requirements it is
fatal at execute (exit 33). In hints it is reported and execution
continues. It is never omitted.

A JavaScript engine is added in the module that uses it, not before.

## Tests

Alcotest only. Invariants in types and signatures first. Example fixtures
and QCheck2 properties via `qcheck-alcotest`. No ppx generators.
`execute_edges` is one runner over fixture files.

Conformance is measured, not asserted:

- `scripts/conformance.py` runs cwltest on `vendor/cwl-v1.2`, and
  `--suite oracle` runs `test/oracle`. With `--baseline REF` it also runs
  the suite against ccr built at REF and fails if a test passing there no
  longer passes. CI uses the PR's base branch.
- A PR that makes ids pass names them, with the spec section it
  implements, in the PR body.
- An expected CWL output comes from the conformance suite or from a
  `test/oracle` case, never from memory. An oracle case quotes the spec
  sentences that pin its output (`ccr:spec`, `ccr:quote`), or says what the
  spec leaves open (`ccr:unspecified`) and takes cwltool's output, recorded
  by `scripts/oracle.py`. New CWL-behavior cases go in `test/oracle`.
  Alcotest fixtures cover internal contracts.
- When cwltool or a v1.2 test contradicts the spec, the spec wins: the
  oracle case keeps the quoted output, the v1.2 test stays failing, and the
  PR quotes the spec. Don't bend ccr to match.
- Done means `dune runtest` passes and both suites pass
  `--baseline origin/main`.
- Reading budget: `_build/conformance/<suite>.status` lists every id with
  its status. Read one failing test's tool and job and the one spec section
  it needs, not a whole spec file.

## Commands

```
git submodule update --init
opam switch create . ocaml-base-compiler.5.5.0 --no-install
dune build && dune runtest
dune exec -- ccr --version
dune fmt
pip install cwltest cwltool                   # conformance and oracle only
scripts/conformance.py [--baseline REF]       # v1.2 suite
scripts/conformance.py --suite oracle         # spec-quoted probes
scripts/oracle.py [--check] [ID ...]          # check authorities, consult cwltool
```
