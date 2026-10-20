include Data.Schema
open Error.Annotated

let diag ?(in_requirements = false) feature path =
  Error.unimplemented ~in_requirements feature path

let child = Untyped_tree.child
let nth path i = Printf.sprintf "%s[%d]" path i
let map_i read path xs = all (List.mapi (fun i -> read (nth path i)) xs)

let expected ~json_path ~what value =
  Error.schema json_path (Untyped_tree.expected what value)

let field ~json_path kvs key read absent =
  match List.assoc_opt key kvs with
  | None -> absent
  | Some v -> read (child json_path key) v

let some read path v =
  let+ x = read path v in
  Some x

(* A plain result, from a reader that finds no diagnostic, as an annotated one. *)
let string_field path kvs key =
  Result.bind (Untyped_tree.string_field ~json_path:path kvs key) return

let parse_cwl_version kvs =
  match List.assoc_opt "cwlVersion" kvs with
  | None -> return "v1.2"
  | Some (Untyped_tree.String (("v1.0" | "v1.1" | "v1.2") as s)) -> return s
  | Some (Untyped_tree.String s) ->
      Error.schema "cwlVersion" ("unsupported cwlVersion " ^ s)
  | Some other -> expected ~json_path:"cwlVersion" ~what:"string" other

let parse_int path = function
  | Untyped_tree.Int n -> return (Int64.to_int n)
  | other -> expected ~json_path:path ~what:"int" other

let parse_position path = function
  | Untyped_tree.String s -> return (Ty.Expr s)
  | Untyped_tree.Int n -> return (Ty.Pos (Int64.to_int n))
  | other -> expected ~json_path:path ~what:"int or expression" other

let strings ~what path = function
  | Untyped_tree.Null -> return []
  | Untyped_tree.String s -> return [ s ]
  | Untyped_tree.Array xs -> (
      let strings, others =
        List.partition_map
          (function Untyped_tree.String s -> Left s | other -> Right other)
          xs
      in
      match others with
      | [] -> return strings
      | bad :: _ -> expected ~json_path:path ~what bad)
  | other -> expected ~json_path:path ~what:(what ^ " or array") other

let shortname id =
  match String.rindex_opt id '#' with
  | None -> Filename.basename id
  | Some i -> String.sub id (i + 1) (String.length id - i - 1)

(* An enum symbol is an IRI; a job names it by its last segment. *)
let symbol s =
  match List.rev (String.split_on_char '/' (shortname s)) with
  | last :: _ -> last
  | [] -> s

let is_namespaced key = String.contains key ':'

(* A diagnostic, made by [report], for each key of [kvs] outside [allowed]. *)
let other_keys allowed report path kvs =
  List.filter_map
    (fun (k, _) ->
      if List.mem k allowed then None else Some (report k (child path k)))
    kvs

let diagnostics_for_keys ~implemented ~unimplemented path kvs =
  let feature k =
    if is_namespaced k then "extension " ^ k
    else if List.mem k unimplemented then k
    else "unknown field " ^ k
  in
  other_keys implemented (fun k -> diag (feature k)) path kvs

let binding_keys =
  [ "position"; "prefix"; "separate"; "itemSeparator"; "valueFrom" ]

let parse_binding path = function
  | Untyped_tree.Object kvs ->
      let text = string_field path kvs in
      let+ () =
        warn
          (diagnostics_for_keys ~implemented:binding_keys
             ~unimplemented:[ "shellQuote"; "loadContents" ]
             path kvs)
          ()
      and+ position =
        field ~json_path:path kvs "position" parse_position
          (return Ty.default_binding.position)
      and+ prefix = text "prefix"
      and+ separate =
        Result.bind
          (Untyped_tree.bool_field ~json_path:path kvs "separate")
          return
      and+ item_separator = text "itemSeparator"
      and+ value_from = text "valueFrom" in
      let separate = Option.value separate ~default:true in
      { Ty.position; prefix; separate; item_separator; value_from }
  | Untyped_tree.Null -> return Ty.default_binding
  | other -> expected ~json_path:path ~what:"inputBinding object" other

(* A list that Schema Salad also accepts as a map (its mapSubject): in map
   form each key is the entry's id or class, and [read] gets it. *)
let parse_entries ~what read path = function
  | Untyped_tree.Null -> return [] (* a null process field is absent *)
  | Untyped_tree.Array xs -> map_i (fun path -> read path None) path xs
  | Untyped_tree.Object kvs ->
      all (List.map (fun (k, v) -> read (child path k) (Some k) v) kvs)
  | other -> expected ~json_path:path ~what other

(* What every input and output parameter has: an id, which a map key gives
   when there is one, and a type, read by [parse_type]; a bare string or list
   is the type alone (map_res.yml: a map item that is not an object is the
   value of its [mapPredicate]). Returns the id, the type, the fields, and the diagnostics for
   its keys, which the caller reports after those of its other fields. *)
let parse_param ~what parse_type ~implemented ~unimplemented path key v =
  let* kvs, type_path =
    match v with
    | Untyped_tree.(String _ | Array _) -> return ([ ("type", v) ], path)
    | Untyped_tree.Object kvs -> return (kvs, child path "type")
    | other -> expected ~json_path:path ~what:(what ^ " parameter") other
  in
  let* id =
    match key with Some _ -> return key | None -> string_field path kvs "id"
  in
  match (id, List.assoc_opt "type" kvs) with
  | None, _ -> Error.schema path (what ^ " missing id")
  | _, None -> Error.schema type_path (what ^ " missing type")
  | Some id, Some t ->
      let+ ty = parse_type type_path t in
      ( shortname id,
        ty,
        kvs,
        diagnostics_for_keys
          ~implemented:("id" :: "label" :: "doc" :: "type" :: implemented)
          ~unimplemented:
            ("secondaryFiles" :: "streamable" :: "loadContents" :: unimplemented)
          path kvs )

(* Process.yml, InputFormat: one or more IRIs, or an expression, which is
   not evaluated. *)
let parse_format path v =
  let* formats = strings ~what:"format IRI" path v in
  if List.exists (fun f -> String.contains f '$') formats then
    warn [ diag "format expression" path ] []
  else return formats

let rec parse_cwl_type path = function
  | Untyped_tree.String s -> parse_type_dsl path s
  | Untyped_tree.Array parts ->
      let+ ts = all (List.map (parse_cwl_type path) parts) in
      Ty.Union ts
  | Untyped_tree.Object kvs -> parse_type_object path kvs
  | other -> expected ~json_path:path ~what:"a CWL type" other

and parse_type_dsl json_path s =
  if String.ends_with ~suffix:"[]" s then
    let inner = String.sub s 0 (String.length s - 2) in
    let+ t = parse_type_dsl json_path inner in
    Ty.Array { items = t; item_binding = None }
  else if String.ends_with ~suffix:"?" s then
    let inner = String.sub s 0 (String.length s - 1) in
    let+ t = parse_type_dsl json_path inner in
    Ty.Union [ t; Ty.Null ]
  else
    match s with
    | "null" -> return Ty.Null
    | "boolean" -> return Ty.Boolean
    | "int" -> return Ty.Int
    | "long" -> return Ty.Long
    | "float" -> return Ty.Float
    | "double" -> return Ty.Double
    | "string" -> return Ty.String
    | "File" -> return Ty.File
    | "Directory" -> return Ty.Directory
    | "Any" -> return Ty.Any
    (* A named type, from a SchemaDefRequirement, is not implemented. No
       value can be checked or bound without it, so the process cannot run. *)
    | other -> Error.unsupported ("type " ^ other)

and parse_type_object path kvs =
  match List.assoc_opt "type" kvs with
  | Some (Untyped_tree.String "array") ->
      let get key = field ~json_path:path kvs key in
      let missing =
        Error.schema (child path "items") "array type missing items"
      in
      let+ items = get "items" parse_cwl_type missing
      and+ item_binding = get "inputBinding" (some parse_binding) (return None)
      and+ () =
        warn
          (other_keys [ "type"; "items"; "inputBinding"; "name" ] diag path kvs)
          ()
      in
      Ty.Array { items; item_binding }
  | Some (Untyped_tree.String "enum") ->
      let missing =
        Error.schema (child path "symbols") "enum type missing symbols"
      in
      let+ symbols =
        field ~json_path:path kvs "symbols" (strings ~what:"symbol") missing
      and+ () =
        warn
          (other_keys
             [ "type"; "symbols"; "name"; "label"; "doc" ]
             diag path kvs)
          ()
      in
      Ty.Enum (List.map symbol symbols)
  | Some (Untyped_tree.String "record") ->
      let+ fields =
        field ~json_path:path kvs "fields"
          (parse_entries ~what:"fields array or map" parse_field)
          (return [])
      and+ () =
        warn
          (other_keys
             [ "type"; "fields"; "name"; "label"; "doc" ]
             diag path kvs)
          ()
      in
      Ty.Record fields
  | Some t -> parse_cwl_type path t
  | None -> Error.schema path "type object missing 'type' field"

(* A record field is a parameter whose id is its [name]. Of an output
   record's field, [outputBinding] is a diagnostic. *)
and parse_field path key v =
  let key =
    match (key, v) with
    | None, Untyped_tree.Object kvs -> (
        match List.assoc_opt "name" kvs with
        | Some (Untyped_tree.String name) -> Some name
        | _ -> None)
    | _ -> key
  in
  let* name, ty, kvs, unimplemented =
    parse_param ~what:"field" parse_cwl_type
      ~implemented:[ "name"; "inputBinding"; "format" ]
      ~unimplemented:[ "outputBinding"; "loadListing" ]
      path key v
  in
  let get key = field ~json_path:path kvs key in
  let+ input_binding = get "inputBinding" (some parse_binding) (return None)
  and+ format = get "format" parse_format (return [])
  and+ () = warn unimplemented () in
  { Ty.name = symbol name; ty; input_binding; format }

let parse_input path key v =
  let* id, ty, kvs, unimplemented =
    parse_param ~what:"input" parse_cwl_type
      ~implemented:[ "default"; "inputBinding"; "format" ]
      ~unimplemented:[ "loadListing" ] path key v
  in
  let get key = field ~json_path:path kvs key in
  let default _ d = Result.bind (Ty.coerce id ty d) return in
  let+ default = get "default" (some default) (return None)
  and+ input_binding = get "inputBinding" (some parse_binding) (return None)
  and+ format = get "format" parse_format (return [])
  and+ () = warn unimplemented () in
  { id; ty; default; input_binding; format; unimplemented }

(* Only [coresMin] is kept, and only as a number: any other value of it (an
   expression, say) is dropped without a diagnostic. Any other field but
   [class] is a diagnostic. *)
let parse_resource path ~in_requirements kvs =
  let cores_min =
    match List.assoc_opt "coresMin" kvs with
    | Some (Untyped_tree.Int n) -> Some (Int64.to_float n)
    | Some (Untyped_tree.Float f) -> Some f
    | _ -> None
  in
  warn
    (other_keys [ "class"; "coresMin" ]
       (fun k -> diag ~in_requirements ("ResourceRequirement." ^ k))
       path kvs)
    (Resource { cores_min })

let docker_sources =
  [ "dockerPull"; "dockerLoad"; "dockerFile"; "dockerImport" ]

let parse_docker path ~in_requirements kvs =
  let take name =
    match List.assoc_opt name kvs with
    | None -> return None
    | Some (Untyped_tree.String s) when s <> "" -> return (Some (name, s))
    | Some (Untyped_tree.String _) ->
        Error.schema path
          ("DockerRequirement " ^ name ^ " must be a non-empty string")
    | Some _ ->
        Error.schema path ("DockerRequirement " ^ name ^ " must be a string")
  in
  let* output_directory =
    match List.assoc_opt "dockerOutputDirectory" kvs with
    | None -> return None
    | Some (Untyped_tree.String s) -> (
        match Container_outdir.of_string s with
        | Ok path -> return (Some path)
        | Error message ->
            Error.schema path
              ("DockerRequirement dockerOutputDirectory " ^ message))
    | Some _ ->
        Error.schema path
          "DockerRequirement dockerOutputDirectory must be a string"
  in
  let* sources = all (List.map take docker_sources) in
  let* image_id = take "dockerImageId" in
  let image_id = Option.map snd image_id in
  let* image =
    match List.filter_map Fun.id sources with
    | [ ("dockerPull", s) ] -> return (Pull s)
    | [ ("dockerImport", s) ] -> return (Import { source = s; name = image_id })
    | [ ("dockerLoad", s) ] -> return (Load { source = s; name = image_id })
    | [ ("dockerFile", s) ] ->
        return (Dockerfile { contents = s; tag = image_id })
    | [] -> (
        match image_id with
        | Some s -> return (Image_id s)
        | None -> Error.schema path "DockerRequirement has no image")
    | _ -> Error.schema path "DockerRequirement has more than one image source"
  in
  let unknown =
    other_keys
      ("class" :: "dockerImageId" :: "dockerOutputDirectory" :: docker_sources)
      (fun k -> diag ("DockerRequirement.unknown field " ^ k))
      path kvs
  in
  let hint =
    if in_requirements then [] else [ diag "DockerRequirement" path ]
  in
  warn (hint @ unknown) (Docker { image; output_directory })

(* In map form the key is the class, and a [null] body has no other field. *)
let parse_requirement ~in_requirements path key v =
  let v =
    match (key, v) with
    | Some class_, Untyped_tree.Object kvs ->
        Untyped_tree.Object (("class", Untyped_tree.String class_) :: kvs)
    | Some class_, Untyped_tree.Null ->
        Untyped_tree.Object [ ("class", Untyped_tree.String class_) ]
    | _ -> v
  in
  match v with
  | Untyped_tree.Object kvs -> (
      match List.assoc_opt "class" kvs with
      | Some (Untyped_tree.String "ResourceRequirement") ->
          parse_resource path ~in_requirements kvs
      | Some (Untyped_tree.String "DockerRequirement") ->
          parse_docker path ~in_requirements kvs
      | Some (Untyped_tree.String class_) ->
          warn
            [ diag ~in_requirements class_ path ]
            (Unimplemented { class_; in_requirements })
      | _ -> Error.schema path "requirement missing class")
  | other -> expected ~json_path:path ~what:"requirement object" other

let parse_req_list ~in_requirements path v =
  let* reqs =
    parse_entries ~what:"requirements/hints list or map"
      (parse_requirement ~in_requirements)
      path v
  in
  if
    List.length (List.filter (function Docker _ -> true | _ -> false) reqs)
    > 1
  then
    Error.schema path
      "DockerRequirement is repeated; the mount target would be ambiguous"
  else return reqs

let first_unimplemented reqs =
  List.find_map
    (function
      | Unimplemented { class_; in_requirements = true } -> Some class_
      | _ -> None)
    reqs

let parse_output_binding path = function
  | Untyped_tree.Object kvs ->
      let unimplemented =
        diagnostics_for_keys ~implemented:[ "glob" ]
          ~unimplemented:[ "loadContents"; "outputEval" ]
          path kvs
      in
      let+ glob =
        field ~json_path:path kvs "glob"
          (strings ~what:"glob string")
          (return [])
      and+ () = warn unimplemented () in
      { glob; unimplemented }
  | Untyped_tree.Null -> return { glob = []; unimplemented = [] }
  | other -> expected ~json_path:path ~what:"outputBinding object" other

let parse_output_type path = function
  | Untyped_tree.String "stdout" -> return (Ty.File, Stdout)
  | Untyped_tree.String "stderr" -> return (Ty.File, Stderr)
  | v ->
      let+ t = parse_cwl_type path v in
      (t, No_stream)

(* Whether a record field in [ty] has a [format]: for an output, which
   assigns it, that is not implemented. *)
let rec field_format = function
  | Ty.Record fields ->
      List.exists
        (fun (f : Ty.field) -> f.format <> [] || field_format f.ty)
        fields
  | Ty.Array { items; _ } -> field_format items
  | Ty.Union ts -> List.exists field_format ts
  | _ -> false

let parse_output path key v =
  let* id, (ty, stream), kvs, unimplemented =
    parse_param ~what:"output" parse_output_type
      ~implemented:[ "outputBinding" ] ~unimplemented:[ "format" ] path key v
  in
  let unimplemented =
    if field_format ty then diag "format" path :: unimplemented
    else unimplemented
  in
  let+ output_binding =
    field ~json_path:path kvs "outputBinding"
      (some parse_output_binding)
      (return None)
  and+ () = warn unimplemented () in
  { id; ty; output_binding; stream; unimplemented }

(* The fields every process class has. Diagnostics come in reading order. *)
let parse_process kvs =
  let list key read = field ~json_path:"" kvs key read (return []) in
  let+ inputs =
    list "inputs" (parse_entries ~what:"inputs array or map" parse_input)
  and+ outputs =
    list "outputs" (parse_entries ~what:"outputs array or map" parse_output)
  and+ requirements = list "requirements" (parse_req_list ~in_requirements:true)
  and+ hints = list "hints" (parse_req_list ~in_requirements:false) in
  (inputs, outputs, requirements, hints)
