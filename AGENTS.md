# caml-cwl-runner

CWL v1.2.1 runner in OCaml 5.5. Binary: `ccr`.
Source of truth, in order: CWL v1.2 spec → conformance tests → cwltool
only for a disputed corner. Not a cwltool port.

CLI: `ccr [tool] [job]`. When execution exists, print the output object
as JSON on stdout. Non-zero on failure; 33 means unimplemented feature.

## Modules

`.mli` is the design. ADTs live once in private `Data` and are `include`d
into `Cwl.Error`, `Cwl.Doc`, `Cwl.Type`, `Cwl.Schema`, `Cwl.Expr`.
Module-dependent functions `(module E : ENGINE) -> …` at effectful holes
(JS eval, FS, process spawn). Stdlib + `Result.t`. No objects, `Obj`,
refs, or Hashtbl unless a Runtime body truly needs them.
I/O only in `Doc` and (when it exists) `Runtime`. Known-unimplemented
CWL is a diagnostic, never a silent drop.

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

Do not add Eio/Lwt, a JS engine, or a vendored cwl-v1.2 tree until the
module that needs them exists.
