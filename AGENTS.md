# caml-cwl-runner

CWL v1.2.1 runner in OCaml 5.5. Binary: `ccr`.

Source of truth, in order: the CWL v1.2 spec (CommandLineTool, Workflow,
Process, `invocation.md`) and the Schema Salad spec, then the v1.2
conformance tests in `vendor/cwl-v1.2`, then cwltool. cwltool is the oracle
for any expected value the suite does not pin. Not a cwltool port: match
its behavior, don't copy its code.

CLI: `ccr [tool] [job]`. On success, print the output object as JSON on
stdout. Exit 33 means an unimplemented feature was required. Any other
failure is exit 1.

## Modules

`.mli` is the contract. ADTs and module signatures (`FS`, `RUNTIME`, `ENGINE`, `FILE`) are defined
once in private `Data` and
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

- `scripts/conformance.py` runs cwltest on `vendor/cwl-v1.2` and ratchets
  `conformance/v1.2.passing`. `--suite oracle` does the same for
  `test/oracle`. A listed id that stops passing fails CI.
- A PR that makes ids pass records them with `--update` in the same PR and
  names them, with the spec section it implements, in the PR body.
- An expected CWL output comes from the conformance suite or from
  `scripts/oracle.py` (cwltool). It is never typed by hand. New CWL-behavior
  cases go in `test/oracle`. Alcotest fixtures cover internal contracts.
- Done means `dune runtest` and both ratchets pass.
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
pip install -r scripts/requirements.txt       # pinned cwltest, cwltool
scripts/conformance.py [--tags T] [--update]  # v1.2 suite
scripts/conformance.py --suite oracle         # cwltool-verified probes
scripts/oracle.py [ID ...]                    # record expected outputs
```
