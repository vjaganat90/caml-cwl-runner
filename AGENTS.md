# caml-cwl-runner

CWL v1.2.1 runner in OCaml 5.5. Binary: `ccr`.
Source of truth, in order: CWL v1.2 spec → conformance tests → cwltool
only for a disputed corner.

CLI: `ccr [tool] [job]`. When execution exists, print the output object
as JSON on stdout. Non-zero on failure; 33 means unimplemented feature.

## Modules

`.mli` is the design. ADTs live once in private `Data` and are `include`d
into `Cwl.Error`, `Cwl.Untyped_tree`, `Cwl.Type`, `Cwl.Schema`, `Cwl.Expr`.
Module-dependent functions `(module E : ENGINE) -> …` at JS eval, FS, and
process spawn. Stdlib + `Result.t`. No objects, `Obj`,
refs, or Hashtbl unless a Runtime body truly needs them.
I/O only in `Untyped_tree` and `Runtime`. Eio is the Runtime body and the CLI
scheduler (`Eio_main.run`). No Lwt. `Glob` is pure given `(module FS)`.
Known-unimplemented CWL is a diagnostic, never a silent drop.

## Tests

Invariants in types and signatures first. Alcotest is the only runner.
Example fixtures plus QCheck2 properties via `qcheck-alcotest`.
No ppx generators.

## Commands

```
opam switch create . ocaml-base-compiler.5.5.0 --no-install
dune build && dune runtest
dune exec -- ccr --version
dune fmt
```

Lwt is not used. A JavaScript engine and a vendored cwl-v1.2 tree belong
in the modules that need them. Eio is confined to Runtime and `bin`.
