include Data.Schema
open Error.Syntax

let diag ?(in_requirements = false) feature path =
  Error.unimplemented ~in_requirements feature path

let child = Untyped_tree.child
let nth path i = Printf.sprintf "%s[%d]" path i

let map_i f xs =
  let rec go i acc diags = function
    | [] -> Ok (List.rev acc, diags)
    | x :: xs ->
        let* v, d = f i x in
        go (i + 1) (v :: acc) (diags @ d) xs
  in
  go 0 [] [] xs

let expected ~json_path ~what value =
  Error.schema json_path (Untyped_tree.expected what value)

let parse_string ~json_path = function
  | Untyped_tree.String s -> Ok s
  | other -> expected ~json_path ~what:"string" other

let parse_cwl_version kvs =
  match List.assoc_opt "cwlVersion" kvs with
  | None -> Ok "v1.2"
  | Some v -> (
      let* s = parse_string ~json_path:"cwlVersion" v in
      match s with
      | "v1.0" | "v1.1" | "v1.2" -> Ok s
      | s -> Error.schema "cwlVersion" ("unsupported cwlVersion " ^ s))

let parse_int json_path = function
  | Untyped_tree.Int n -> Ok (Int64.to_int n)
  | Untyped_tree.Float f when Float.is_integer f -> Ok (int_of_float f)
  | other -> expected ~json_path ~what:"int" other

let parse_position ~json_path = function
  | Untyped_tree.String s -> Ok (Ty.Expr s)
  | other -> (
      match parse_int json_path other with
      | Ok n -> Ok (Ty.Pos n)
      | Error _ -> expected ~json_path ~what:"int or expression" other)

let field parse ~json_path kvs key =
  match List.assoc_opt key kvs with
  | None -> Ok (None, [])
  | Some v ->
      let+ x, diags = parse ~json_path:(child json_path key) v in
      (Some x, diags)

let strings ~json_path ~expected:what xs =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | Untyped_tree.String s :: rest -> go (s :: acc) rest
    | other :: _ -> expected ~json_path ~what other
  in
  go [] xs

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
  | None | Some Untyped_tree.Null -> Ok ([], [])
  | Some v -> parse ~json_path:key v

let shortname id =
  match String.rindex_opt id '#' with
  | None -> Filename.basename id
  | Some i -> String.sub id (i + 1) (String.length id - i - 1)

let is_namespaced key = String.contains key ':'

(* One pair of key lists per kind of object. [implemented_*] keys need no
   diagnostic, even those whose value is dropped (like [label] and [doc]).
   [known_*] adds the CWL fields ccr does not implement. [diagnostics_for_keys]
   reports each key outside [implemented_*] as known, unknown or an extension. *)
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

let rec parse_binding ~json_path v =
  match v with
  | Untyped_tree.Object kvs ->
      let diags =
        diagnostics_for_keys ~implemented:implemented_binding_keys
          ~known:known_binding_keys ~json_path kvs
      in
      let position =
        match List.assoc_opt "position" kvs with
        | None -> Ok Ty.default_binding.position
        | Some v -> parse_position ~json_path:(json_path ^ ".position") v
      in
      let* position in
      let* prefix = Untyped_tree.string_field ~json_path kvs "prefix" in
      let* separate = Untyped_tree.bool_field ~json_path kvs "separate" in
      let separate = Option.value separate ~default:true in
      let* item_separator =
        Untyped_tree.string_field ~json_path kvs "itemSeparator"
      in
      let* value_from = Untyped_tree.string_field ~json_path kvs "valueFrom" in
      Ok ({ Ty.position; prefix; separate; item_separator; value_from }, diags)
  | Untyped_tree.Null -> Ok (Ty.default_binding, [])
  | other -> expected ~json_path ~what:"inputBinding object" other

and parse_cwl_type ~json_path v =
  match v with
  | Untyped_tree.String s -> parse_type_dsl ~json_path s
  | Untyped_tree.Array parts ->
      let+ ts, diags = map_i (fun _i x -> parse_cwl_type ~json_path x) parts in
      (Ty.Union ts, diags)
  | Untyped_tree.Object kvs -> parse_type_object ~json_path kvs
  | other -> expected ~json_path ~what:"a CWL type" other

and parse_type_dsl ~json_path s =
  if String.ends_with ~suffix:"[]" s then
    let inner = String.sub s 0 (String.length s - 2) in
    let+ t, diags = parse_type_dsl ~json_path inner in
    (Ty.Array { items = t; item_binding = None }, diags)
  else if String.ends_with ~suffix:"?" s then
    let inner = String.sub s 0 (String.length s - 1) in
    let+ t, diags = parse_type_dsl ~json_path inner in
    (Ty.Union [ t; Ty.Null ], diags)
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
      let* items, item_diags =
        match List.assoc_opt "items" kvs with
        | None -> Error.schema (json_path ^ ".items") "array type missing items"
        | Some v -> parse_cwl_type ~json_path:(json_path ^ ".items") v
      in
      let* item_binding, bind_diags =
        field parse_binding ~json_path kvs "inputBinding"
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
  | None -> Error.schema json_path "type object missing 'type' field"

(* What every input and output parameter has: an id, which a map key gives
   when there is one, and a type node with its path. A bare string in map form
   is the type alone. Returns the id, the type node, its path and the fields. *)
let parse_param what ~json_path id_opt v =
  let* kvs, type_path =
    match v with
    | Untyped_tree.String _ -> Ok ([ ("type", v) ], json_path)
    | Untyped_tree.Object kvs -> Ok (kvs, child json_path "type")
    | other -> expected ~json_path ~what:(what ^ " parameter") other
  in
  let* id =
    match id_opt with
    | Some id -> Ok (Some id)
    | None -> Untyped_tree.string_field ~json_path kvs "id"
  in
  match (id, List.assoc_opt "type" kvs) with
  | None, _ -> Error.schema json_path (what ^ " missing id")
  | _, None -> Error.schema type_path (what ^ " missing type")
  | Some id, Some t -> Ok (shortname id, t, type_path, kvs)

(* A parameter list: an array of parameters with ids, or a map from id to
   parameter. *)
let parse_params what parse ~json_path = function
  | Untyped_tree.Array xs ->
      map_i (fun i x -> parse what ~json_path:(nth json_path i) None x) xs
  | Untyped_tree.Object kvs ->
      map_assoc
        (fun k v -> parse what ~json_path:(child json_path k) (Some k) v)
        kvs
  | other -> expected ~json_path ~what:(what ^ "s array or map") other

let parse_input what ~json_path id_opt v =
  let* id, t, type_path, kvs = parse_param what ~json_path id_opt v in
  let* ty, ty_diags = parse_cwl_type ~json_path:type_path t in
  let* default =
    Option.fold ~none:(Ok None)
      ~some:(fun d ->
        let+ v = Ty.value_of_tree id ty d in
        Some v)
      (List.assoc_opt "default" kvs)
  in
  let+ input_binding, bind_diags =
    field parse_binding ~json_path kvs "inputBinding"
  in
  let unimplemented =
    diagnostics_for_keys ~implemented:implemented_input_keys
      ~known:known_input_keys ~json_path kvs
  in
  ( { id; ty; default; input_binding; unimplemented },
    ty_diags @ bind_diags @ unimplemented )

let parse_inputs = parse_params "input" parse_input

(* Only [coresMin] is kept, and only as a number: any other value of it (an
   expression, say) is dropped without a diagnostic. Any other field but
   [class] is a diagnostic. *)
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

let parse_requirement ~json_path ~in_requirements v =
  match v with
  | Untyped_tree.Object kvs -> (
      match List.assoc_opt "class" kvs with
      | Some (Untyped_tree.String "ResourceRequirement") ->
          Ok (parse_resource ~json_path ~in_requirements kvs)
      | Some (Untyped_tree.String "DockerRequirement") ->
          let take name =
            match List.assoc_opt name kvs with
            | None -> Ok None
            | Some (Untyped_tree.String s) when s <> "" -> Ok (Some s)
            | Some (Untyped_tree.String _) ->
                Error.schema json_path
                  ("DockerRequirement " ^ name ^ " must be a non-empty string")
            | Some _ ->
                Error.schema json_path
                  ("DockerRequirement " ^ name ^ " must be a string")
          in
          let* output_directory =
            match List.assoc_opt "dockerOutputDirectory" kvs with
            | None -> Ok None
            | Some (Untyped_tree.String s) -> (
                match Container_outdir.of_string s with
                | Ok path -> Ok (Some path)
                | Error message ->
                    Error.schema json_path
                      ("DockerRequirement dockerOutputDirectory " ^ message))
            | Some _ ->
                Error.schema json_path
                  "DockerRequirement dockerOutputDirectory must be a string"
          in
          let* pull = take "dockerPull" in
          let* load = take "dockerLoad" in
          let* file = take "dockerFile" in
          let* import_src = take "dockerImport" in
          let* image_id = take "dockerImageId" in
          let sources =
            List.filter_map
              (fun (name, value) -> Option.map (fun s -> (name, s)) value)
              [
                ("dockerPull", pull);
                ("dockerImport", import_src);
                ("dockerLoad", load);
                ("dockerFile", file);
              ]
          in
          let unknown =
            List.filter_map
              (fun (k, _) ->
                if
                  List.mem k
                    [
                      "class";
                      "dockerPull";
                      "dockerLoad";
                      "dockerFile";
                      "dockerImport";
                      "dockerImageId";
                      "dockerOutputDirectory";
                    ]
                then None
                else
                  Some
                    (diag ~in_requirements:false
                       ("DockerRequirement.unknown field " ^ k)
                       (child json_path k)))
              kvs
          in
          let image =
            match sources with
            | [ ("dockerPull", s) ] -> Ok (Pull s)
            | [ ("dockerImport", s) ] ->
                Ok (Import { source = s; name = image_id })
            | [ ("dockerLoad", s) ] -> Ok (Load { source = s; name = image_id })
            | [ ("dockerFile", s) ] ->
                Ok (Dockerfile { contents = s; tag = image_id })
            | [] -> (
                match image_id with
                | Some s -> Ok (Image_id s)
                | None ->
                    Error.schema json_path "DockerRequirement has no image")
            | _ ->
                Error.schema json_path
                  "DockerRequirement has more than one image source"
          in
          let* image = image in
          let docker = { image; output_directory } in
          if in_requirements then Ok (Docker docker, unknown)
          else
            let d = diag ~in_requirements:false "DockerRequirement" json_path in
            Ok (Docker docker, d :: unknown)
      | Some (Untyped_tree.String class_) ->
          let diag = diag ~in_requirements class_ json_path in
          Ok (Unimplemented { class_; in_requirements }, [ diag ])
      | _ -> Error.schema json_path "requirement missing class")
  | other -> expected ~json_path ~what:"requirement object" other

let one_docker json_path (reqs, diags) =
  let n =
    List.length (List.filter (function Docker _ -> true | _ -> false) reqs)
  in
  if n > 1 then
    Error.schema json_path
      "DockerRequirement is repeated; the mount target would be ambiguous"
  else Ok (reqs, diags)

let parse_req_list ~json_path ~in_requirements v =
  match v with
  | Untyped_tree.Null -> Ok ([], [])
  | Untyped_tree.Array xs ->
      let* parsed =
        map_i
          (fun i x ->
            parse_requirement ~json_path:(nth json_path i) ~in_requirements x)
          xs
      in
      one_docker json_path parsed
  | Untyped_tree.Object kvs ->
      let* parsed =
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
      in
      one_docker json_path parsed
  | other -> expected ~json_path ~what:"requirements/hints list or map" other

let first_unimplemented reqs =
  List.find_map
    (function
      | Unimplemented { class_; in_requirements = true } -> Some class_
      | _ -> None)
    reqs

let parse_glob_list ~json_path v =
  match v with
  | Untyped_tree.Null -> Ok []
  | Untyped_tree.String s -> Ok [ s ]
  | Untyped_tree.Array xs -> strings ~json_path ~expected:"glob string" xs
  | other -> expected ~json_path ~what:"glob string or array" other

let parse_output_binding ~json_path v =
  match v with
  | Untyped_tree.Object kvs ->
      let diags =
        diagnostics_for_keys ~implemented:implemented_output_binding_keys
          ~known:known_output_binding_keys ~json_path kvs
      in
      let* glob =
        Option.fold ~none:(Ok [])
          ~some:(parse_glob_list ~json_path:(child json_path "glob"))
          (List.assoc_opt "glob" kvs)
      in
      Ok ({ glob; unimplemented = diags }, diags)
  | Untyped_tree.Null -> Ok ({ glob = []; unimplemented = [] }, [])
  | other -> expected ~json_path ~what:"outputBinding object" other

let parse_output_type ~json_path v =
  match v with
  | Untyped_tree.String "stdout" -> Ok (Ty.File, Stdout, [])
  | Untyped_tree.String "stderr" -> Ok (Ty.File, Stderr, [])
  | _ ->
      let+ t, d = parse_cwl_type ~json_path v in
      (t, No_stream, d)

let parse_output what ~json_path id_opt v =
  let* id, t, type_path, kvs = parse_param what ~json_path id_opt v in
  let* ty, stream, ty_diags = parse_output_type ~json_path:type_path t in
  let+ output_binding, bind_diags =
    field parse_output_binding ~json_path kvs "outputBinding"
  in
  let unimplemented =
    diagnostics_for_keys ~implemented:implemented_output_keys
      ~known:known_output_keys ~json_path kvs
  in
  ( { id; ty; output_binding; stream; unimplemented },
    ty_diags @ bind_diags @ unimplemented )

let parse_outputs = parse_params "output" parse_output

(* The fields every process class has. Diagnostics come in reading order. *)
let parse_process kvs =
  let* inputs, in_diags = parse_opt parse_inputs "inputs" kvs in
  let* outputs, out_diags = parse_opt parse_outputs "outputs" kvs in
  let* requirements, req_diags =
    parse_opt (parse_req_list ~in_requirements:true) "requirements" kvs
  in
  let+ hints, hint_diags =
    parse_opt (parse_req_list ~in_requirements:false) "hints" kvs
  in
  ( (inputs, outputs, requirements, hints),
    in_diags @ out_diags @ req_diags @ hint_diags )
