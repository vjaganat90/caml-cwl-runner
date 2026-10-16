(** Laws of the value walk in [Cwl.Type]: walking with the identity rebuilds the
    same value, so arrays and records keep their shape and key order. *)

open QCheck2

let value =
  let open Cwl.Type in
  Gen.(
    sized_size (int_range 0 3)
    @@ fix (fun self n ->
        let leaf =
          oneof
            [
              pure Vnull;
              map (fun i -> Vint (Int64.of_int i)) nat;
              map (fun s -> Vstring s) string_small;
            ]
        in
        let list g = list_size (int_range 0 3) g in
        if n = 0 then leaf
        else
          oneof
            [
              leaf;
              map (fun xs -> Varray xs) (list (self (n - 1)));
              map
                (fun kvs -> Vrecord kvs)
                (list (pair string_small (self (n - 1))));
            ]))

let identity =
  Test.make ~name:"walking with the identity rebuilds the value" ~count:200
    value (fun v ->
      Cwl.Type.map Fun.id v = v
      && Cwl.Type.map_result Result.ok v = Ok v
      && Cwl.Type.fold_map (fun acc x -> Ok (x, acc)) () v = Ok (v, ()))

let tests =
  [
    ("type_walk", [ QCheck_alcotest.to_alcotest ~speed_level:`Quick identity ]);
  ]
