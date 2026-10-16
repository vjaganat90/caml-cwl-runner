include Data.Workflow
open Error.Syntax

let of_tree tree =
  match tree with
  | Untyped_tree.Object kvs ->
      let* cwl_version = Schema.parse_cwl_version kvs in
      let+ (inputs, outputs, requirements, hints), diagnostics =
        Schema.parse_process kvs
      in
      {
        Error.value = { cwl_version; inputs; outputs; requirements; hints };
        diagnostics = Error.unimplemented "Workflow" "class" :: diagnostics;
      }
  | other -> Schema.expected ~json_path:"/" ~what:"a CWL document object" other
