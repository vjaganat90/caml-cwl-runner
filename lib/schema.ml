(** CommandLineTool and a Workflow stub as typed OCaml. Known-unimplemented
    fields become diagnostics; they are not dropped. Does not evaluate
    expressions or spawn. *)

include Data.Schema

let ( let* ) = Error.( let* )
let schema_err path message = Error (Error.Schema { path; message })

let diag ?(in_requirements = false) feature path =
  Error.unimplemented ~in_requirements feature path

let child path name = if path = "" then name else path ^ "." ^ name
let nth path i = Printf.sprintf "%s[%d]" path i

let map_i f xs =
  let rec go i acc diags = function
    | [] -> Ok (List.rev acc, diags)
    | x :: xs ->
        let* v, d = f i x in
        go (i + 1) (v :: acc) (diags @ d) xs
  in
  go 0 [] [] xs

let map_assoc f kvs =
  let rec go acc diags = function
    | [] -> Ok (List.rev acc, diags)
    | (k, v) :: rest ->
        let* x, d = f k v in
        go (x :: acc) (diags @ d) rest
  in
  go [] [] kvs

let parse_opt parse key kvs =
  match List.assoc_opt key kvs with
  | None -> Ok ([], [])
  | Some v -> parse ~json_path:key v

let shortname id =
  match String.rindex_opt id '#' with
  | None -> Filename.basename id
  | Some i -> String.sub id (i + 1) (String.length id - i - 1)

let is_namespaced key = String.contains key ':'

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
    "$graph";
    "$import";
    "$include";
    "successCodes";
  ]

let known_tool_keys =
  implemented_tool_keys
  @ [
      "intent";
      "temporaryFailCodes";
      "permanentFailCodes";
      "$namespaces";
      "$schemas";
      "$base";
    ]

let implemented_input_keys =
  [ "id"; "label"; "doc"; "type"; "default"; "inputBinding" ]

let known_input_keys =
  implemented_input_keys
  @ [ "secondaryFiles"; "format"; "streamable"; "loadContents"; "loadListing" ]

let implemented_binding_keys =
  [ "position"; "prefix"; "separate"; "itemSeparator"; "valueFrom" ]

let known_binding_keys =
  implemented_binding_keys @ [ "shellQuote"; "loadContents" ]

let implemented_output_keys = [ "id"; "label"; "doc"; "type"; "outputBinding" ]

let known_output_keys =
  implemented_output_keys
  @ [ "secondaryFiles"; "format"; "streamable"; "loadContents" ]

let implemented_output_binding_keys = [ "glob" ]

let known_output_binding_keys =
  implemented_output_binding_keys @ [ "loadContents"; "outputEval" ]

let diagnostics_for_keys ~implemented ~known ~json_path kvs =
  List.filter_map
    (fun (k, _) ->
      if List.mem k implemented then None
      else if is_namespaced k then
        Some (diag ("extension " ^ k) (child json_path k))
      else if List.mem k known then Some (diag k (child json_path k))
      else Some (diag ("unknown field " ^ k) (child json_path k)))
    kvs

let rec parse_binding ~json_path v :
    (binding * Error.diagnostic list, Error.t) result =
  match v with
  | Untyped_tree.Object kvs ->
      let diags =
        diagnostics_for_keys ~implemented:implemented_binding_keys
          ~known:known_binding_keys ~json_path kvs
      in
      let position =
        match List.assoc_opt "position" kvs with
        | None -> Ok Ty.default_binding.position
        | Some (Untyped_tree.Int n) -> Ok (Ty.Pos (Int64.to_int n))
        | Some (Untyped_tree.Float f) when Float.is_integer f ->
            Ok (Ty.Pos (int_of_float f))
        | Some (Untyped_tree.String s) -> Ok (Ty.Expr s)
        | Some other ->
            schema_err (json_path ^ ".position")
              (Format.asprintf "expected int or expression, got %a"
                 Untyped_tree.pp other)
      in
      let* position in
      let prefix = Untyped_tree.string_field kvs "prefix" in
      let separate =
        Option.value (Untyped_tree.bool_field kvs "separate") ~default:true
      in
      let item_separator = Untyped_tree.string_field kvs "itemSeparator" in
      let value_from = Untyped_tree.string_field kvs "valueFrom" in
      Ok ({ Ty.position; prefix; separate; item_separator; value_from }, diags)
  | Untyped_tree.Null -> Ok (Ty.default_binding, [])
  | other ->
      schema_err json_path
        (Format.asprintf "expected inputBinding object, got %a" Untyped_tree.pp
           other)

and parse_cwl_type ~json_path v :
    (cwl_type * Error.diagnostic list, Error.t) result =
  match v with
  | Untyped_tree.String s -> parse_type_dsl ~json_path s
  | Untyped_tree.Array parts ->
      let rec go acc diags = function
        | [] -> Ok (Ty.Union (List.rev acc), diags)
        | x :: xs -> (
            match parse_cwl_type ~json_path x with
            | Error _ as e -> e
            | Ok (t, d) -> go (t :: acc) (diags @ d) xs)
      in
      go [] [] parts
  | Untyped_tree.Object kvs -> parse_type_object ~json_path kvs
  | other ->
      schema_err json_path
        (Format.asprintf "expected a CWL type, got %a" Untyped_tree.pp other)

and parse_type_dsl ~json_path s =
  if String.ends_with ~suffix:"[]" s then
    let inner = String.sub s 0 (String.length s - 2) in
    let* t, diags = parse_type_dsl ~json_path inner in
    Ok (Ty.Array { items = t; item_binding = None }, diags)
  else if String.ends_with ~suffix:"?" s then
    let inner = String.sub s 0 (String.length s - 1) in
    let* t, diags = parse_type_dsl ~json_path inner in
    Ok (Ty.Union [ t; Ty.Null ], diags)
  else
    match s with
    | "null" -> Ok (Ty.Null, [])
    | "boolean" -> Ok (Ty.Boolean, [])
    | "int" -> Ok (Ty.Int, [])
    | "long" -> Ok (Ty.Long, [])
    | "float" -> Ok (Ty.Float, [])
    | "double" -> Ok (Ty.Double, [])
    | "string" -> Ok (Ty.String, [])
    | "File" -> Ok (Ty.File, [])
    | "Directory" -> Ok (Ty.Directory, [])
    | "Any" -> Ok (Ty.String, [ diag "Any" json_path ])
    | other -> Ok (Ty.String, [ diag ("type " ^ other) json_path ])

and parse_type_object ~json_path kvs =
  match List.assoc_opt "type" kvs with
  | Some (Untyped_tree.String "array") ->
      let items =
        match List.assoc_opt "items" kvs with
        | None -> schema_err (json_path ^ ".items") "array type missing items"
        | Some v -> parse_cwl_type ~json_path:(json_path ^ ".items") v
      in
      let* items, item_diags = items in
      let* item_binding, bind_diags =
        match List.assoc_opt "inputBinding" kvs with
        | None -> Ok (None, [])
        | Some v ->
            let* b, d =
              parse_binding ~json_path:(json_path ^ ".inputBinding") v
            in
            Ok (Some b, d)
      in
      let extra =
        List.filter_map
          (fun (k, _) ->
            if List.mem k [ "type"; "items"; "inputBinding"; "name" ] then None
            else Some (diag k (child json_path k)))
          kvs
      in
      Ok (Ty.Array { items; item_binding }, item_diags @ bind_diags @ extra)
  | Some (Untyped_tree.String ("record" | "enum")) as some_t ->
      let feature =
        match some_t with
        | Some (Untyped_tree.String s) -> s
        | _ -> "complex type"
      in
      Ok (Ty.String, [ diag feature json_path ])
  | Some t -> parse_cwl_type ~json_path t
  | None -> schema_err json_path "type object missing 'type' field"

let parse_default ~param ~ty v = Ty.value_of_tree param ty v

let parse_input ~json_path ~id_opt v :
    (input * Error.diagnostic list, Error.t) result =
  match v with
  | Untyped_tree.String s ->
      let id = match id_opt with Some id -> id | None -> "" in
      if id = "" then schema_err json_path "input missing id"
      else
        let* ty, diags = parse_type_dsl ~json_path s in
        Ok
          ( {
              id = shortname id;
              ty;
              default = None;
              input_binding = None;
              unimplemented = [];
            },
            diags )
  | Untyped_tree.Object kvs ->
      let id =
        match id_opt with
        | Some id -> Some id
        | None -> Untyped_tree.string_field kvs "id"
      in
      let* id =
        match id with
        | Some id -> Ok (shortname id)
        | None -> schema_err json_path "input missing id"
      in
      let* ty, ty_diags =
        match List.assoc_opt "type" kvs with
        | None -> schema_err (child json_path "type") "input missing type"
        | Some t -> parse_cwl_type ~json_path:(child json_path "type") t
      in
      let* default =
        match List.assoc_opt "default" kvs with
        | None -> Ok None
        | Some d ->
            let* v = parse_default ~param:id ~ty d in
            Ok (Some v)
      in
      let* input_binding, bind_diags =
        match List.assoc_opt "inputBinding" kvs with
        | None -> Ok (None, [])
        | Some b ->
            let* b, d =
              parse_binding ~json_path:(child json_path "inputBinding") b
            in
            Ok (Some b, d)
      in
      let unimplemented =
        diagnostics_for_keys ~implemented:implemented_input_keys
          ~known:known_input_keys ~json_path kvs
      in
      Ok
        ( { id; ty; default; input_binding; unimplemented },
          ty_diags @ bind_diags @ unimplemented )
  | other ->
      schema_err json_path
        (Format.asprintf "expected input parameter, got %a" Untyped_tree.pp
           other)

let parse_inputs ~json_path v :
    (input list * Error.diagnostic list, Error.t) result =
  match v with
  | Untyped_tree.Array xs ->
      map_i
        (fun i x -> parse_input ~json_path:(nth json_path i) ~id_opt:None x)
        xs
  | Untyped_tree.Object kvs ->
      map_assoc
        (fun k v ->
          parse_input ~json_path:(child json_path k) ~id_opt:(Some k) v)
        kvs
  | other ->
      schema_err json_path
        (Format.asprintf "expected inputs array or map, got %a" Untyped_tree.pp
           other)

let parse_arguments ~json_path v :
    (argument list * Error.diagnostic list, Error.t) result =
  match v with
  | Untyped_tree.Null -> Ok ([], [])
  | Untyped_tree.Array xs ->
      map_i
        (fun i x ->
          match x with
          | Untyped_tree.String s -> Ok (Literal s, [])
          | _ ->
              let* b, d = parse_binding ~json_path:(nth json_path i) x in
              Ok (Binding b, d))
        xs
  | other ->
      schema_err json_path
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
            schema_err "baseCommand"
              (Format.asprintf "expected string, got %a" Untyped_tree.pp other)
      in
      go [] xs
  | Untyped_tree.Null -> Ok []
  | other ->
      schema_err "baseCommand"
        (Format.asprintf "expected string or array, got %a" Untyped_tree.pp
           other)

let parse_resource ~json_path ~in_requirements kvs =
  let cores_min =
    match List.assoc_opt "coresMin" kvs with
    | Some (Untyped_tree.Int n) -> Some (Int64.to_float n)
    | Some (Untyped_tree.Float f) -> Some f
    | _ -> None
  in
  let extra =
    List.filter_map
      (fun (k, _) ->
        if List.mem k [ "class"; "coresMin" ] then None
        else
          Some
            (diag ~in_requirements
               ("ResourceRequirement." ^ k)
               (child json_path k)))
      kvs
  in
  (Resource { cores_min }, extra)

let parse_requirement ~json_path ~in_requirements v :
    (requirement * Error.diagnostic list, Error.t) result =
  match v with
  | Untyped_tree.Object kvs -> (
      match List.assoc_opt "class" kvs with
      | Some (Untyped_tree.String "ResourceRequirement") ->
          Ok (parse_resource ~json_path ~in_requirements kvs)
      | Some (Untyped_tree.String class_) ->
          let diag = diag ~in_requirements class_ json_path in
          Ok (Unimplemented { class_; in_requirements }, [ diag ])
      | _ -> schema_err json_path "requirement missing class")
  | other ->
      schema_err json_path
        (Format.asprintf "expected requirement object, got %a" Untyped_tree.pp
           other)

let parse_req_list ~json_path ~in_requirements v :
    (requirement list * Error.diagnostic list, Error.t) result =
  match v with
  | Untyped_tree.Null -> Ok ([], [])
  | Untyped_tree.Array xs ->
      map_i
        (fun i x ->
          parse_requirement ~json_path:(nth json_path i) ~in_requirements x)
        xs
  | Untyped_tree.Object kvs ->
      map_assoc
        (fun class_ body ->
          let path = child json_path class_ in
          let body =
            match body with
            | Untyped_tree.Object fields ->
                Untyped_tree.Object
                  (("class", Untyped_tree.String class_) :: fields)
            | Untyped_tree.Null ->
                Untyped_tree.Object [ ("class", Untyped_tree.String class_) ]
            | other -> other
          in
          parse_requirement ~json_path:path ~in_requirements body)
        kvs
  | other ->
      schema_err json_path
        (Format.asprintf "expected requirements/hints list or map, got %a"
           Untyped_tree.pp other)

let parse_glob_list ~json_path v =
  match v with
  | Untyped_tree.Null -> Ok []
  | Untyped_tree.String s -> Ok [ s ]
  | Untyped_tree.Array xs ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | Untyped_tree.String s :: rest -> go (s :: acc) rest
        | other :: _ ->
            schema_err json_path
              (Format.asprintf "expected glob string, got %a" Untyped_tree.pp
                 other)
      in
      go [] xs
  | other ->
      schema_err json_path
        (Format.asprintf "expected glob string or array, got %a" Untyped_tree.pp
           other)

let parse_output_binding ~json_path v :
    (output_binding * Error.diagnostic list, Error.t) result =
  match v with
  | Untyped_tree.Object kvs ->
      let diags =
        diagnostics_for_keys ~implemented:implemented_output_binding_keys
          ~known:known_output_binding_keys ~json_path kvs
      in
      let* glob =
        match List.assoc_opt "glob" kvs with
        | None -> Ok []
        | Some g -> parse_glob_list ~json_path:(child json_path "glob") g
      in
      Ok ({ glob; unimplemented = diags }, diags)
  | Untyped_tree.Null -> Ok ({ glob = []; unimplemented = [] }, [])
  | other ->
      schema_err json_path
        (Format.asprintf "expected outputBinding object, got %a" Untyped_tree.pp
           other)

let parse_output_type ~json_path v =
  match v with
  | Untyped_tree.String "stdout" -> Ok (Ty.File, Stdout, [])
  | Untyped_tree.String "stderr" -> Ok (Ty.File, Stderr, [])
  | _ ->
      let* t, d = parse_cwl_type ~json_path v in
      Ok (t, No_stream, d)

let parse_output ~json_path ~id_opt v :
    (output * Error.diagnostic list, Error.t) result =
  match v with
  | Untyped_tree.String s ->
      let id = match id_opt with Some id -> id | None -> "" in
      if id = "" then schema_err json_path "output missing id"
      else
        let* ty, stream, diags =
          parse_output_type ~json_path (Untyped_tree.String s)
        in
        Ok
          ( {
              id = shortname id;
              ty;
              output_binding = None;
              stream;
              unimplemented = [];
            },
            diags )
  | Untyped_tree.Object kvs ->
      let id =
        match id_opt with
        | Some id -> Some id
        | None -> Untyped_tree.string_field kvs "id"
      in
      let* id =
        match id with
        | Some id -> Ok (shortname id)
        | None -> schema_err json_path "output missing id"
      in
      let* ty, stream, ty_diags =
        match List.assoc_opt "type" kvs with
        | None -> schema_err (child json_path "type") "output missing type"
        | Some t -> parse_output_type ~json_path:(child json_path "type") t
      in
      let* output_binding, bind_diags =
        match List.assoc_opt "outputBinding" kvs with
        | None -> Ok (None, [])
        | Some b ->
            let* b, d =
              parse_output_binding
                ~json_path:(child json_path "outputBinding")
                b
            in
            Ok (Some b, d)
      in
      let unimplemented =
        diagnostics_for_keys ~implemented:implemented_output_keys
          ~known:known_output_keys ~json_path kvs
      in
      Ok
        ( { id; ty; output_binding; stream; unimplemented },
          ty_diags @ bind_diags @ unimplemented )
  | other ->
      schema_err json_path
        (Format.asprintf "expected output parameter, got %a" Untyped_tree.pp
           other)

let parse_outputs ~json_path v :
    (output list * Error.diagnostic list, Error.t) result =
  match v with
  | Untyped_tree.Null -> Ok ([], [])
  | Untyped_tree.Array xs ->
      map_i
        (fun i x -> parse_output ~json_path:(nth json_path i) ~id_opt:None x)
        xs
  | Untyped_tree.Object kvs ->
      map_assoc
        (fun k v ->
          parse_output ~json_path:(child json_path k) ~id_opt:(Some k) v)
        kvs
  | other ->
      schema_err json_path
        (Format.asprintf "expected outputs array or map, got %a" Untyped_tree.pp
           other)

let parse_success_codes v =
  let one path x =
    match x with
    | Untyped_tree.Int n -> Ok (Int64.to_int n)
    | Untyped_tree.Float f when Float.is_integer f -> Ok (int_of_float f)
    | other ->
        schema_err path
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
            let* n = one (nth "successCodes" i) x in
            go (i + 1) (n :: acc) xs
      in
      go 0 [] xs
  | other ->
      schema_err "successCodes"
        (Format.asprintf "expected int or array of int, got %a" Untyped_tree.pp
           other)

let command_line_tool doc =
  match doc with
  | Untyped_tree.Array _ ->
      schema_err "/" "$graph / packed documents are not implemented"
  | Untyped_tree.Object kvs ->
      let class_ =
        Option.value (Untyped_tree.string_field kvs "class") ~default:""
      in
      let cwl_version =
        Option.value
          (Untyped_tree.string_field kvs "cwlVersion")
          ~default:"v1.2"
      in
      let top_diags =
        diagnostics_for_keys ~implemented:implemented_tool_keys
          ~known:known_tool_keys ~json_path:"" kvs
      in
      let class_diag =
        if class_ = "CommandLineTool" || class_ = "" then []
        else [ diag class_ "class" ]
      in
      let* base_command =
        match List.assoc_opt "baseCommand" kvs with
        | None -> Ok []
        | Some v -> parse_base_command v
      in
      let* arguments, arg_diags = parse_opt parse_arguments "arguments" kvs in
      let* inputs, in_diags = parse_opt parse_inputs "inputs" kvs in
      let* outputs, out_diags = parse_opt parse_outputs "outputs" kvs in
      let* requirements, req_diags =
        parse_opt (parse_req_list ~in_requirements:true) "requirements" kvs
      in
      let* hints, hint_diags =
        parse_opt (parse_req_list ~in_requirements:false) "hints" kvs
      in
      let stdout = Untyped_tree.string_field kvs "stdout" in
      let stdin = Untyped_tree.string_field kvs "stdin" in
      let stderr = Untyped_tree.string_field kvs "stderr" in
      let* success_codes =
        match List.assoc_opt "successCodes" kvs with
        | None -> Ok [ 0 ]
        | Some v -> parse_success_codes v
      in
      let present k =
        match List.assoc_opt k kvs with Some _ -> [ diag k k ] | None -> []
      in
      let graph_diag = present "$graph" in
      let import_diag = present "$import" @ present "$include" in
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
        @ hint_diags @ graph_diag @ import_diag
      in
      Ok { Error.value = tool; diagnostics }
  | other ->
      schema_err "/"
        (Format.asprintf "expected a CWL document object, got %a"
           Untyped_tree.pp other)

let workflow tree =
  match tree with
  | Untyped_tree.Object kvs ->
      let cwl_version =
        Option.value
          (Untyped_tree.string_field kvs "cwlVersion")
          ~default:"v1.2"
      in
      Ok
        {
          Error.value = { cwl_version; class_ = "Workflow" };
          diagnostics = [ diag "Workflow" "class" ];
        }
  | other ->
      schema_err "/"
        (Format.asprintf "expected a CWL document object, got %a"
           Untyped_tree.pp other)

let document tree =
  match tree with
  | Untyped_tree.Array _ ->
      schema_err "/" "$graph / packed documents are not implemented"
  | Untyped_tree.Object kvs -> (
      match
        Option.value (Untyped_tree.string_field kvs "class") ~default:""
      with
      | "Workflow" ->
          let* wf = workflow tree in
          Ok
            {
              Error.value = Document.Workflow wf.value;
              diagnostics = wf.diagnostics;
            }
      | "CommandLineTool" | "" ->
          let* clt = command_line_tool tree in
          Ok
            {
              Error.value = Document.CommandLineTool clt.value;
              diagnostics = clt.diagnostics;
            }
      | class_ -> Error (Error.Unsupported { feature = class_ }))
  | other ->
      schema_err "/"
        (Format.asprintf "expected a CWL document object, got %a"
           Untyped_tree.pp other)

let cores_min tool =
  let from_list rs =
    List.find_map
      (function Resource { cores_min } -> cores_min | _ -> None)
      rs
  in
  match from_list tool.requirements with
  | Some _ as c -> c
  | None -> from_list tool.hints

let input_specs tool =
  List.map
    (fun (i : input) -> { Ty.id = i.id; ty = i.ty; default = i.default })
    tool.inputs
