(** Argv examples and binding properties. Does not spawn and does not parse
    fixture documents. *)

open Harness
open QCheck2

let example_cases =
  [
    example_case ~docker:true "cl_basic_generation" "bwa-mem-tool.cwl"
      "bwa-mem-job.json"
      [
        "bwa";
        "mem";
        "-t";
        "2";
        "-I";
        "1,2,3,4";
        "-m";
        "3";
        "chr20.fa";
        "example_human_Illumina.pe_1.fastq";
        "example_human_Illumina.pe_2.fastq";
      ];
    example_case ~docker:true "cl_optional_inputs_missing" "cat1-testcli.cwl"
      "cat-job.json" [ "cat"; "hello.txt" ];
    example_case "cl_optional_bindings_provided" "cat1-testcli.cwl"
      "cat-n-job.json"
      [ "cat"; "-n"; "hello.txt" ];
    example_case "nested_prefixes_arrays" "binding-test.cwl" "bwa-mem-job.json"
      [
        "bwa";
        "mem";
        "chr20.fa";
        "-XXX";
        "-YYY";
        "example_human_Illumina.pe_1.fastq";
        "-YYY";
        "example_human_Illumina.pe_2.fastq";
      ];
  ]

let prop_boolean_flag =
  Test.make ~name:"boolean flag" ~count:100 Gen.bool (fun b ->
      argv_ok
        [ bound "flag" ~ty:Cwl.Type.Boolean ~prefix:"-f" ]
        [ ("flag", Cwl.Type.Vbool b) ]
        (if b then [ "echo"; "-f" ] else [ "echo" ]))

let prop_optional_null_silent =
  Test.make ~name:"optional null is silent" ~count:20 Gen.unit (fun () ->
      argv_ok
        [
          bound "msg"
            ~ty:(Cwl.Type.Union [ Cwl.Type.String; Cwl.Type.Null ])
            ~prefix:"--msg";
        ]
        [ ("msg", Cwl.Type.Vnull) ]
        [ "echo" ])

let prop_item_separator =
  Test.make ~name:"itemSeparator joins" ~count:50
    Gen.(list_size (1 -- 5) (string_size ~gen:printable (1 -- 4)))
    (fun xs ->
      argv_ok
        [
          bound "arr"
            ~ty:
              (Cwl.Type.Array { items = Cwl.Type.String; item_binding = None })
            ~prefix:"-I" ~item_separator:",";
        ]
        [ ("arr", strings xs) ]
        [ "echo"; "-I"; String.concat "," xs ])

let prop_separate_false =
  Test.make ~name:"separate:false concatenates" ~count:50
    Gen.(string_size ~gen:printable (1 -- 8))
    (fun s ->
      argv_ok
        [ bound "v" ~prefix:"-i" ~separate:false ]
        [ ("v", Cwl.Type.Vstring s) ]
        [ "echo"; "-i" ^ s ])

let prop_base_command_prefix =
  Test.make ~name:"argv starts with baseCommand" ~count:30
    Gen.(list_size (1 -- 3) (string_size ~gen:printable (1 -- 6)))
    (fun base -> argv_ok ~base_command:base [] [] base)

let prop_position_order =
  Test.make ~name:"numeric position order" ~count:50
    Gen.(pair (int_range (-5) 5) (int_range (-5) 5))
    (fun (p1, p2) ->
      match
        argv
          [ bound "a" ~position:p1; bound "b" ~position:p2 ]
          [ ("a", Cwl.Type.Vstring "A"); ("b", Cwl.Type.Vstring "B") ]
      with
      | Error _ -> false
      | Ok argv ->
          let rest = match argv with "echo" :: r -> r | r -> r in
          if p1 < p2 then rest = [ "A"; "B" ]
          else if p2 < p1 then rest = [ "B"; "A" ]
          else rest = [ "A"; "B" ] || rest = [ "B"; "A" ])

let tests =
  [
    ("bind_examples", example_cases);
    ( "bind_properties",
      List.map
        (QCheck_alcotest.to_alcotest ~speed_level:`Quick)
        [
          prop_boolean_flag;
          prop_optional_null_silent;
          prop_item_separator;
          prop_separate_false;
          prop_base_command_prefix;
          prop_position_order;
        ] );
  ]
