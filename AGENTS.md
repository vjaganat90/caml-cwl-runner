# caml-cwl-runner

CWL v1.2.1 runner in OCaml 5.5. Binary: `ccr`.

Spec, then the v1.2 conformance tests, then cwltool only for a disputed
corner. Not a cwltool port.

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
`Glob` is pure given `(module FS)`.

An unimplemented CWL construct is a `diagnostic`. In requirements it is
fatal at execute (exit 33). In hints it is reported and execution
continues. It is never omitted.

A JavaScript engine and a vendored `cwl-v1.2` tree are added in the module
that uses them, not before.

## Tests

Alcotest only. Invariants in types and signatures first. Example fixtures
and QCheck2 properties via `qcheck-alcotest`. No ppx generators.
`execute_edges` is one runner over fixture files.

## Commands

```
opam switch create . ocaml-base-compiler.5.5.0 --no-install
dune build && dune runtest
dune exec -- ccr --version
dune fmt
```
