include Data.Workflow
open Error.Annotated

let of_tree tree =
  match tree with
  | Untyped_tree.Object kvs ->
      let+ () = warn [ Error.unimplemented "Workflow" "class" ] ()
      and+ cwl_version = Schema.parse_cwl_version kvs
      and+ inputs, outputs, requirements, hints = Schema.parse_process kvs in
      { cwl_version; inputs; outputs; requirements; hints }
  | other -> Schema.expected ~json_path:"/" ~what:"a CWL document object" other
