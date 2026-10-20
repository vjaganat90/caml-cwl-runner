(** Laws of the value walk in [Cwl.Type]: walking with the identity rebuilds the
    same value, so arrays and records keep their shape and key order. *)

open QCheck2

let value =
  Gen.(
    sized_size (int_range 0 3)
    @@ fix (fun self n ->
        let leaf =
          oneof
            [
              pure Cwl.Untyped_tree.Null;
              map (fun i -> Cwl.Untyped_tree.Int (Int64.of_int i)) nat;
              map (fun s -> Cwl.Untyped_tree.String s) string_small;
            ]
        in
        let list g = list_size (int_range 0 3) g in
        if n = 0 then leaf
        else
          oneof
            [
              leaf;
              map (fun xs -> Cwl.Untyped_tree.Array xs) (list (self (n - 1)));
              map
                (fun kvs -> Cwl.Untyped_tree.Object kvs)
                (list (pair string_small (self (n - 1))));
            ]))

let identity =
  Test.make ~name:"walking with the identity rebuilds the value" ~count:200
    value (fun v ->
      Cwl.Type.map Fun.id v = v
      && Cwl.Type.map_result Result.ok v = Ok v
      && Cwl.Type.fold_map (fun acc x -> Ok (x, acc)) () v = Ok (v, ()))

(* Process.yml, File and Directory: [format], [contents], [secondaryFiles] and
   [listing] are given, not computed, so checking a job and looking them up in
   an expression keep them as given; a missing [basename] is computed from the
   [location]. *)
let prop_fields_kept =
  let keys = [ "format"; "contents"; "secondaryFiles"; "listing" ] in
  Test.make ~name:"a checked File or Directory keeps its fields" ~count:200
    Gen.(triple (oneof_list [ "File"; "Directory" ]) (oneof_list keys) value)
    (fun (cls, key, x) ->
      let ty : Cwl.Type.t = if cls = "File" then File else Directory in
      let f =
        Cwl.Untyped_tree.Object
          [
            ("class", Cwl.Untyped_tree.String cls);
            ("location", Cwl.Untyped_tree.String "/d/a.txt");
            (key, x);
          ]
      in
      let spec = { Cwl.Type.id = "f"; ty; default = None } in
      match Cwl.Type.apply_defaults_and_check [ spec ] [ ("f", f) ] with
      | Ok inputs ->
          let runtime = Cwl.Expr.default_runtime in
          let ctx =
            { Cwl.Expr.inputs; self = Cwl.Untyped_tree.Null; runtime }
          in
          let eval k = Cwl.Expr.Param_ref.eval ctx ("$(inputs.f." ^ k ^ ")") in
          eval key = Ok x
          && eval "basename" = Ok (Cwl.Untyped_tree.String "a.txt")
      | Error _ -> false)

(* Which job values a declared type takes: the input [x] of the type, as
   YAML, its value, and the check's result. *)
let type_rows =
  [
    ("Any", "3", "ok");
    ("Any", "{a: [1]}", "ok");
    ("Any", "null", "missing required input parameter 'x'");
  ]

let type_table () =
  let check (ty, value, want) =
    let open Cwl.Error.Syntax in
    let yaml s = Cwl.Untyped_tree.of_yaml_string s in
    (let* tool =
       yaml ("class: CommandLineTool\noutputs: []\ninputs: {x: " ^ ty ^ "}")
     in
     let* tool = Cwl.Command_line_tool.of_tree tool in
     let* job = Result.bind (yaml ("x: " ^ value)) Cwl.Type.object_of_tree in
     Cwl.Type.apply_defaults_and_check
       (Cwl.Command_line_tool.input_specs tool.value)
       job)
    |> Result.fold ~ok:(fun _ -> "ok") ~error:Cwl.Error.to_string
    |> Alcotest.(check string) (ty ^ " takes " ^ value) want
  in
  List.iter check type_rows

let tests =
  [
    ( "type_walk",
      ("type_table", `Quick, type_table)
      :: List.map
           (QCheck_alcotest.to_alcotest ~speed_level:`Quick)
           [ identity; prop_fields_kept ] );
  ]
