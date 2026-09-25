(** Alcotest entry. Cases live in the other test modules. *)

let () =
  Alcotest.run "cwl"
    (Test_bind.tests @ Test_expr.tests @ Test_glob.tests @ Test_import.tests
   @ Test_docker.tests @ Test_runtime.tests @ Test_execute.tests)
