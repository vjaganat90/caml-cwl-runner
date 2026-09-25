(** A CWL Workflow record. Parsing only: graph execution is not here yet.
    [of_tree] accepts [class: Workflow] and reports it unimplemented. *)

include Data.Workflow

let ( let* ) = Error.( let* )

let of_tree tree =
  match tree with
  | Untyped_tree.Object kvs ->
      let* cwl_version = Schema.parse_cwl_version kvs in
      let* inputs, in_diags =
        Schema.parse_opt Schema.parse_inputs "inputs" kvs
      in
      let* outputs, out_diags =
        Schema.parse_opt Schema.parse_outputs "outputs" kvs
      in
      let* requirements, req_diags =
        Schema.parse_opt
          (Schema.parse_req_list ~in_requirements:true)
          "requirements" kvs
      in
      let* hints, hint_diags =
        Schema.parse_opt
          (Schema.parse_req_list ~in_requirements:false)
          "hints" kvs
      in
      Ok
        {
          Error.value =
            {
              cwl_version;
              class_ = "Workflow";
              inputs;
              outputs;
              requirements;
              hints;
            };
          diagnostics =
            [ Error.unimplemented "Workflow" "class" ]
            @ in_diags @ out_diags @ req_diags @ hint_diags;
        }
  | other ->
      Schema.schema_err "/"
        (Format.asprintf "expected a CWL document object, got %a"
           Untyped_tree.pp other)
