(** Parameter references and string interpolation. Does not spawn. *)

open QCheck2

let prop_param_ref_cores =
  Test.make ~name:"$(runtime.cores) roundtrips" ~count:40
    Gen.(int_range 1 16)
    (fun n ->
      let ctx =
        {
          Cwl.Expr.inputs = [];
          self = Cwl.Untyped_tree.Null;
          runtime = Cwl.Expr.runtime_with_cores (float_of_int n);
        }
      in
      match Cwl.Expr.Param_ref.eval ctx "$(runtime.cores)" with
      | Ok (Cwl.Untyped_tree.Int k) -> Int64.to_int k = n
      | Ok (Cwl.Untyped_tree.Float f) -> int_of_float f = n
      | _ -> false)

let interp_ctx inputs =
  {
    Cwl.Expr.inputs;
    self = Cwl.Untyped_tree.Null;
    runtime = Cwl.Expr.default_runtime;
  }

let prop_interpolate =
  let lit =
    Gen.map
      (fun s -> `Lit s)
      Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 0 8))
  in
  let atom =
    Gen.oneof
      [
        lit;
        Gen.return `Bs;
        Gen.return `Paren;
        Gen.return `Brace;
        Gen.return `Msg;
      ]
  in
  let encode = function
    | `Lit s -> s
    | `Bs -> "\\\\"
    | `Paren -> "\\$("
    | `Brace -> "\\${"
    | `Msg -> "$(inputs.msg)"
  in
  let decoded = function
    | `Lit s -> s
    | `Bs -> "\\"
    | `Paren -> "$("
    | `Brace -> "${"
    | `Msg -> "hi"
  in
  Test.make ~name:"interpolation round-trips escapes and references" ~count:80
    Gen.(list_size (int_range 0 12) atom)
    (fun atoms ->
      let src = "p" ^ String.concat "" (List.map encode atoms) ^ "s" in
      let expect = "p" ^ String.concat "" (List.map decoded atoms) ^ "s" in
      let ctx = interp_ctx [ ("msg", Cwl.Untyped_tree.String "hi") ] in
      match Cwl.Expr.Param_ref.eval ctx src with
      | Ok (Cwl.Untyped_tree.String got) -> got = expect
      | _ -> false)

let show_eval = function
  | Ok v -> Cwl.Type.string_of_value v
  | Error e -> Cwl.Error.to_string e

let interpolation_table () =
  let eval inputs expr = Cwl.Expr.Param_ref.eval (interp_ctx inputs) expr in
  let msg = [ ("msg", Cwl.Untyped_tree.String "hi") ] in
  let n = [ ("n", Cwl.Untyped_tree.Int 3L) ] in
  let obj =
    [
      ( "o",
        Cwl.Untyped_tree.Object
          [ ("b", Cwl.Untyped_tree.Int 1L); ("a", Cwl.Untyped_tree.Int 2L) ] );
    ]
  in
  (match eval n "  $(inputs.n)  " with
  | Ok (Cwl.Untyped_tree.Int 3L) -> ()
  | other -> Alcotest.failf "lone reference: %s" (show_eval other));
  (match eval msg "$(inputs.msg) tail" with
  | Ok (Cwl.Untyped_tree.String "hi tail") -> ()
  | other -> Alcotest.failf "trailing text: %s" (show_eval other));
  (match eval obj "pre$(inputs.o)" with
  | Ok (Cwl.Untyped_tree.String got) ->
      Alcotest.(check string) "sorted keys" "pre{\"a\":2,\"b\":1}" got
  | other -> Alcotest.failf "object: %s" (show_eval other));
  (match eval msg "\\$(inputs.msg)" with
  | Ok (Cwl.Untyped_tree.String "$(inputs.msg)") -> ()
  | other -> Alcotest.failf "escape: %s" (show_eval other));
  (match eval [] "${return 1}" with
  | Error (Cwl.Error.Unsupported { feature = "InlineJavascriptRequirement" }) ->
      ()
  | other -> Alcotest.failf "javascript: %s" (show_eval other));
  (match eval msg "$(inputs.msg" with
  | Error (Cwl.Error.Expr _) -> ()
  | other -> Alcotest.failf "unclosed: %s" (show_eval other));
  let xs = [ ("xs", Cwl.Untyped_tree.Array [ Cwl.Untyped_tree.Int 1L ]) ] in
  match eval xs "$(inputs.xs[99999999999999999999])" with
  | Error (Cwl.Error.Expr _) -> ()
  | other -> Alcotest.failf "index past max_int: %s" (show_eval other)

let tests =
  [
    ( "expr",
      ("table", `Quick, interpolation_table)
      :: List.map
           (QCheck_alcotest.to_alcotest ~speed_level:`Quick)
           [ prop_param_ref_cores; prop_interpolate ] );
  ]
