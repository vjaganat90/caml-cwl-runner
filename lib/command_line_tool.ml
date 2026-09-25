(** A CWL CommandLineTool record and its parser. Known-unimplemented fields
    become diagnostics; they are not dropped. Does not evaluate expressions or
    spawn. *)

include Data.Command_line_tool

let ( let* ) = Error.( let* )

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

let parse_arguments ~json_path v :
    (argument list * Error.diagnostic list, Error.t) result =
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
  | other ->
      Schema.schema_err json_path
        (Format.asprintf "expected arguments array, got %a" Untyped_tree.pp
           other)

let parse_base_command v =
  match v with
  | Untyped_tree.String s -> Ok [ s ]
  | Untyped_tree.Array xs ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | Untyped_tree.String s :: xs -> go (s :: acc) xs
        | other :: _ ->
            Schema.schema_err "baseCommand"
              (Format.asprintf "expected string, got %a" Untyped_tree.pp other)
      in
      go [] xs
  | Untyped_tree.Null -> Ok []
  | other ->
      Schema.schema_err "baseCommand"
        (Format.asprintf "expected string or array, got %a" Untyped_tree.pp
           other)

let parse_success_codes v =
  let one path x =
    match x with
    | Untyped_tree.Int n -> Ok (Int64.to_int n)
    | Untyped_tree.Float f when Float.is_integer f -> Ok (int_of_float f)
    | other ->
        Schema.schema_err path
          (Format.asprintf "expected int, got %a" Untyped_tree.pp other)
  in
  match v with
  | Untyped_tree.Null -> Ok [ 0 ]
  | Untyped_tree.Int _ | Untyped_tree.Float _ ->
      let* n = one "successCodes" v in
      Ok [ n ]
  | Untyped_tree.Array xs ->
      let rec go i acc = function
        | [] -> Ok (List.rev acc)
        | x :: xs ->
            let* n = one (Schema.nth "successCodes" i) x in
            go (i + 1) (n :: acc) xs
      in
      go 0 [] xs
  | other ->
      Schema.schema_err "successCodes"
        (Format.asprintf "expected int or array of int, got %a" Untyped_tree.pp
           other)

let of_tree doc =
  match doc with
  | Untyped_tree.Array _ ->
      Schema.schema_err "/" "$graph / packed documents are not implemented"
  | Untyped_tree.Object kvs ->
      let class_ =
        Option.value (Untyped_tree.string_field kvs "class") ~default:""
      in
      let* cwl_version = Schema.parse_cwl_version kvs in
      let* () =
        let forbidden k =
          match List.assoc_opt k kvs with
          | None -> Ok ()
          | Some _ -> Schema.schema_err k ("unresolved " ^ k)
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
        match List.assoc_opt "baseCommand" kvs with
        | None -> Ok []
        | Some v -> parse_base_command v
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
      let stdout = Untyped_tree.string_field kvs "stdout" in
      let stdin = Untyped_tree.string_field kvs "stdin" in
      let stderr = Untyped_tree.string_field kvs "stderr" in
      let* success_codes =
        match List.assoc_opt "successCodes" kvs with
        | None -> Ok [ 0 ]
        | Some v -> parse_success_codes v
      in
      let tool =
        {
          cwl_version;
          class_;
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
  | other ->
      Schema.schema_err "/"
        (Format.asprintf "expected a CWL document object, got %a"
           Untyped_tree.pp other)

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
