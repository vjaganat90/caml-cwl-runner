(lang dune 3.17)

(name caml-cwl-runner)

(version 0.1.0)

(license Apache-2.0)

(generate_opam_files true)

(source
 (github portmac/caml-cwl-runner))

(authors "caml-cwl-runner contributors")

(maintainers "caml-cwl-runner contributors")

(package
 (name caml-cwl-runner)
 (synopsis "A CWL v1.2 runner in OCaml")
 (description
  "Common Workflow Language v1.2.1 runner. Executes CommandLineTool locally.")
 (depends
  (ocaml
   (>= 5.5.0))
  (dune
   (>= 3.17))
  cmdliner
  yaml
  re
  eio
  eio_main
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)))
