(** A CWL CommandLineTool record and its parser. Known-unimplemented fields
    become diagnostics; they are not dropped. Does not evaluate expressions or
    spawn. *)

include Data.Command_line_tool
open Error.Syntax

let implemented_tool_keys =
  [
    "cwlVersion";
    "class";
    "id";
    "label";
    "doc";
    "inputs";
    "outputs";
    "baseCommand";
    "arguments";
    "stdin";
    "stdout";
    "stderr";
    "requirements";
    "hints";
    "successCodes";
    "$namespaces";
    "$schemas";
    "$base";
  ]

let known_tool_keys =
  implemented_tool_keys
  @ [ "intent"; "temporaryFailCodes"; "permanentFailCodes" ]

let parse_arguments ~json_path v =
  match v with
  | Untyped_tree.Null -> Ok ([], [])
  | Untyped_tree.Array xs ->
      Schema.map_i
        (fun i x ->
          match x with
          | Untyped_tree.String s -> Ok (Literal s, [])
          | _ ->
              let* b, d =
                Schema.parse_binding ~json_path:(Schema.nth json_path i) x
              in
              Ok (Binding b, d))
        xs
  | other -> Schema.expected ~json_path ~what:"arguments array" other

let parse_base_command v =
  match v with
  | Untyped_tree.String s -> Ok [ s ]
  | Untyped_tree.Array xs ->
      Schema.strings ~json_path:"baseCommand" ~expected:"string" xs
  | Untyped_tree.Null -> Ok []
  | other ->
      Schema.expected ~json_path:"baseCommand" ~what:"string or array" other

let parse_success_codes v =
  let one = Schema.parse_int in
  match v with
  | Untyped_tree.Null -> Ok [ 0 ]
  | Untyped_tree.Int _ | Untyped_tree.Float _ ->
      let+ n = one "successCodes" v in
      [ n ]
  | Untyped_tree.Array xs ->
      let+ ns, _ =
        Schema.map_i
          (fun i x ->
            let+ n = one (Schema.nth "successCodes" i) x in
            (n, []))
          xs
      in
      ns
  | other ->
      Schema.expected ~json_path:"successCodes" ~what:"int or array of int"
        other

let of_tree doc =
  match doc with
  | Untyped_tree.Array _ ->
      Error.schema "/" "$graph / packed documents are not implemented"
  | Untyped_tree.Object kvs ->
      let* class_ = Untyped_tree.string_field ~json_path:"" kvs "class" in
      let class_ = Option.value class_ ~default:"" in
      let* cwl_version = Schema.parse_cwl_version kvs in
      let* () =
        let forbidden k =
          match List.assoc_opt k kvs with
          | None -> Ok ()
          | Some _ -> Error.schema k ("unresolved " ^ k)
        in
        let* () = forbidden "$import" in
        let* () = forbidden "$include" in
        forbidden "$graph"
      in
      let top_diags =
        Schema.diagnostics_for_keys ~implemented:implemented_tool_keys
          ~known:known_tool_keys ~json_path:"" kvs
      in
      let class_diag =
        if class_ = "CommandLineTool" || class_ = "" then []
        else [ Schema.diag class_ "class" ]
      in
      let* base_command =
        Option.fold ~none:(Ok []) ~some:parse_base_command
          (List.assoc_opt "baseCommand" kvs)
      in
      let* arguments, arg_diags =
        Schema.parse_opt parse_arguments "arguments" kvs
      in
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
      let* stdout = Untyped_tree.string_field ~json_path:"" kvs "stdout" in
      let* stdin = Untyped_tree.string_field ~json_path:"" kvs "stdin" in
      let* stderr = Untyped_tree.string_field ~json_path:"" kvs "stderr" in
      let* success_codes =
        Option.fold ~none:(Ok [ 0 ]) ~some:parse_success_codes
          (List.assoc_opt "successCodes" kvs)
      in
      let tool =
        {
          cwl_version;
          base_command;
          arguments;
          inputs;
          outputs;
          stdout;
          stdin;
          stderr;
          success_codes;
          requirements;
          hints;
        }
      in
      let diagnostics =
        top_diags @ class_diag @ arg_diags @ in_diags @ out_diags @ req_diags
        @ hint_diags
      in
      Ok { Error.value = tool; diagnostics }
  | other -> Schema.expected ~json_path:"/" ~what:"a CWL document object" other

let cores_min tool =
  let from_list rs =
    List.find_map
      (function Schema.Resource { cores_min } -> cores_min | _ -> None)
      rs
  in
  match from_list tool.requirements with
  | Some _ as c -> c
  | None -> from_list tool.hints

let input_specs tool =
  List.map
    (fun (i : Schema.input) -> { Ty.id = i.id; ty = i.ty; default = i.default })
    tool.inputs
