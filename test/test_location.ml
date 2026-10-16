(** Reading a location as a path. Pure: no file is read. *)

(* A location is a URI reference: RFC 3986, 2.1 percent-encoding. *)
let to_path_rows =
  [
    ("file:///d/a%20b.txt", "/d/a b.txt");
    ("A%3AGln2Cys", "A:Gln2Cys");
    ("octothorpe/item %231.txt", "octothorpe/item #1.txt");
    ("%61%2f", "a/");
    ("100%", "100%");
    ("%G1%4", "%G1%4");
  ]

let to_path_case (loc, want) =
  ( loc,
    `Quick,
    fun () -> Alcotest.(check string) loc want (Cwl.Location.to_path loc) )

let tests = [ ("location", List.map to_path_case to_path_rows) ]
