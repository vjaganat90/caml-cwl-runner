include Data.Type

let default_binding =
  {
    position = Pos 0;
    prefix = None;
    separate = true;
    item_separator = None;
    value_from = None;
  }

let rec is_optional : t -> bool = function
  | Null -> true
  | Union ts -> List.exists is_optional ts
  | _ -> false

let rec type_name = function
  | Null -> "null"
  | Boolean -> "boolean"
  | Int -> "int"
  | Long -> "long"
  | Float -> "float"
  | Double -> "double"
  | String -> "string"
  | File -> "File"
  | Directory -> "Directory"
  | Array { items; _ } -> type_name items ^ "[]"
  | Union ts -> "[" ^ String.concat ", " (List.map type_name ts) ^ "]"

let value_kind = function
  | Vnull -> "null"
  | Vbool _ -> "boolean"
  | Vint _ -> "int"
  | Vfloat _ -> "float"
  | Vstring _ -> "string"
  | Vfile _ -> "File"
  | Vdir _ -> "Directory"
  | Varray _ -> "array"
  | Vrecord _ -> "record"

let file_basename_of location_or_path =
  match location_or_path with
  | None -> None
  | Some p ->
      let base = Filename.basename p in
      if base = "" then None else Some base

let split_name basename =
  match String.rindex_opt basename '.' with
  | None | Some 0 -> (basename, "")
  | Some i ->
      ( String.sub basename 0 i,
        String.sub basename i (String.length basename - i) )

let fill_file (f : file) =
  let path =
    match f.path with Some _ as p -> p | None -> file_basename_of f.location
  in
  let basename =
    match f.basename with
    | Some _ as b -> b
    | None -> (
        match path with Some p -> Some (Filename.basename p) | None -> None)
  in
  let nameroot, nameext =
    match basename with
    | None -> (f.nameroot, f.nameext)
    | Some b -> (
        let nr, ne = split_name b in
        ( (match f.nameroot with Some _ as x -> x | None -> Some nr),
          match f.nameext with Some _ as x -> x | None -> Some ne ))
  in
  { f with path; basename; nameroot; nameext }

let rec fill_file_paths = function
  | Vfile f -> Vfile (fill_file f)
  | Vdir d ->
      let path =
        match d.path with
        | Some _ as p -> p
        | None -> file_basename_of d.location
      in
      Vdir { d with path }
  | Varray xs -> Varray (List.map fill_file_paths xs)
  | Vrecord kvs -> Vrecord (List.map (fun (k, v) -> (k, fill_file_paths v)) kvs)
  | v -> v

let lookup key obj = List.assoc_opt key obj

let rec matches ty value =
  match (ty, value) with
  | Null, Vnull -> true
  | Boolean, Vbool _ -> true
  | (Int | Long), Vint _ -> true
  | (Int | Long), Vfloat f when Float.is_integer f -> true
  | (Float | Double), Vfloat _ -> true
  | (Float | Double), Vint _ -> true
  | String, Vstring _ -> true
  | File, Vfile _ -> true
  | Directory, Vdir _ -> true
  | Array { items; _ }, Varray xs -> List.for_all (matches items) xs
  | Union ts, v -> List.exists (fun t -> matches t v) ts
  | _, Vrecord _ -> false
  | _ -> false

let rec parse_file_object ~param kvs =
  let class_ =
    match List.assoc_opt "class" kvs with
    | Some (Doc.String "File") | None -> Ok ()
    | Some v ->
        Error
          (Error.Type
             { param; expected = "File"; got = Format.asprintf "%a" Doc.pp v })
  in
  match class_ with
  | Error _ as e -> e
  | Ok () ->
      Ok
        (Vfile
           (fill_file
              {
                location = Doc.string_field kvs "location";
                path = Doc.string_field kvs "path";
                basename = Doc.string_field kvs "basename";
                nameroot = Doc.string_field kvs "nameroot";
                nameext = Doc.string_field kvs "nameext";
                checksum = Doc.string_field kvs "checksum";
                size = Doc.int_field kvs "size";
              }))

and parse_directory_object ~param kvs =
  let class_ =
    match List.assoc_opt "class" kvs with
    | Some (Doc.String "Directory") | None -> Ok ()
    | Some v ->
        Error
          (Error.Type
             {
               param;
               expected = "Directory";
               got = Format.asprintf "%a" Doc.pp v;
             })
  in
  match class_ with
  | Error _ as e -> e
  | Ok () ->
      Ok
        (fill_file_paths
           (Vdir
              {
                location = Doc.string_field kvs "location";
                path = Doc.string_field kvs "path";
              }))

and value_of_doc ~param ~ty doc =
  let ( let* ) = Error.( let* ) in
  let fail expected =
    Error
      (Error.Type { param; expected; got = Format.asprintf "%a" Doc.pp doc })
  in
  match (ty, doc) with
  | Union ts, doc ->
      let rec try_ts = function
        | [] -> fail (type_name ty)
        | t :: rest -> (
            match value_of_doc ~param ~ty:t doc with
            | Ok v -> Ok v
            | Error _ -> try_ts rest)
      in
      (* Prefer a non-null match when the document is not null. *)
      let ordered =
        match doc with
        | Doc.Null ->
            List.filter (function Null -> true | _ -> false) ts
            @ List.filter (function Null -> false | _ -> true) ts
        | _ ->
            List.filter (function Null -> false | _ -> true) ts
            @ List.filter (function Null -> true | _ -> false) ts
      in
      try_ts ordered
  | Null, Doc.Null -> Ok Vnull
  | Boolean, Doc.Bool b -> Ok (Vbool b)
  | (Int | Long), Doc.Int n -> Ok (Vint n)
  | (Int | Long), Doc.Float f when Float.is_integer f ->
      Ok (Vint (Int64.of_float f))
  | (Float | Double), Doc.Float f -> Ok (Vfloat f)
  | (Float | Double), Doc.Int n -> Ok (Vfloat (Int64.to_float n))
  | String, Doc.String s -> Ok (Vstring s)
  | String, Doc.Int n -> Ok (Vstring (Int64.to_string n))
  | String, Doc.Float f -> Ok (Vstring (string_of_float f))
  | File, Doc.Object kvs -> parse_file_object ~param kvs
  | Directory, Doc.Object kvs -> parse_directory_object ~param kvs
  | Array { items; _ }, Doc.Array xs ->
      let* xs = Error.map_list (value_of_doc ~param ~ty:items) xs in
      Ok (Varray xs)
  | _, Doc.Null -> if is_optional ty then Ok Vnull else fail (type_name ty)
  | _ -> fail (type_name ty)

let object_of_doc = function
  | Doc.Null -> Ok []
  | Doc.Object kvs ->
      (* Loose decode: files/dirs recognized by class, else generic. *)
      let rec of_any = function
        | Doc.Null -> Vnull
        | Doc.Bool b -> Vbool b
        | Doc.Int n -> Vint n
        | Doc.Float f -> Vfloat f
        | Doc.String s -> Vstring s
        | Doc.Array xs -> Varray (List.map of_any xs)
        | Doc.Object kvs -> (
            match List.assoc_opt "class" kvs with
            | Some (Doc.String "File") -> (
                match parse_file_object ~param:"job" kvs with
                | Ok v -> v
                | Error _ ->
                    Vrecord (List.map (fun (k, v) -> (k, of_any v)) kvs))
            | Some (Doc.String "Directory") -> (
                match parse_directory_object ~param:"job" kvs with
                | Ok v -> v
                | Error _ ->
                    Vrecord (List.map (fun (k, v) -> (k, of_any v)) kvs))
            | _ -> Vrecord (List.map (fun (k, v) -> (k, of_any v)) kvs))
      in
      Ok (List.map (fun (k, v) -> (k, of_any v)) kvs)
  | other ->
      Error
        (Error.Schema
           {
             path = "/";
             message =
               Printf.sprintf "job must be a YAML/JSON object, got %s"
                 (Format.asprintf "%a" Doc.pp other);
           })

let apply_defaults_and_check ~inputs ~job =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | spec :: rest -> (
        let provided = lookup spec.id job in
        let raw =
          match provided with
          | Some Vnull | None -> (
              match spec.default with
              | Some d -> Ok (Some (fill_file_paths d))
              | None when is_optional spec.ty -> Ok (Some Vnull)
              | None -> Error (Error.Missing { param = spec.id }))
          | Some v -> Ok (Some v)
        in
        match raw with
        | Error _ as e -> e
        | Ok None -> go acc rest
        | Ok (Some v) ->
            if
              matches spec.ty v
              || match v with Vnull -> is_optional spec.ty | _ -> false
            then go ((spec.id, fill_file_paths v) :: acc) rest
            else
              Error
                (Error.Type
                   {
                     param = spec.id;
                     expected = type_name spec.ty;
                     got = value_kind v;
                   }))
  in
  go [] inputs

let rec string_of_value = function
  | Vnull -> "null"
  | Vbool true -> "true"
  | Vbool false -> "false"
  | Vint n -> Int64.to_string n
  | Vfloat f ->
      if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f
      else
        let s = Printf.sprintf "%.10f" f in
        let rec trim s =
          if String.length s > 1 && s.[String.length s - 1] = '0' then
            trim (String.sub s 0 (String.length s - 1))
          else if String.length s > 1 && s.[String.length s - 1] = '.' then
            s ^ "0"
          else s
        in
        trim s
  | Vstring s -> s
  | Vfile f -> (
      match f.path with
      | Some p -> p
      | None -> Option.value f.location ~default:"")
  | Vdir d -> (
      match d.path with
      | Some p -> p
      | None -> Option.value d.location ~default:"")
  | Varray xs -> "[" ^ String.concat ", " (List.map string_of_value xs) ^ "]"
  | Vrecord kvs ->
      "{"
      ^ String.concat ", "
          (List.map (fun (k, v) -> k ^ ": " ^ string_of_value v) kvs)
      ^ "}"

let json_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let json_object fields =
  let body =
    String.concat "," (List.map (fun (k, v) -> json_string k ^ ":" ^ v) fields)
  in
  "{" ^ body ^ "}"

let rec to_json = function
  | Vnull -> "null"
  | Vbool true -> "true"
  | Vbool false -> "false"
  | Vint n -> Int64.to_string n
  | Vfloat f ->
      if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f
      else string_of_float f
  | Vstring s -> json_string s
  | Vfile f ->
      let fields = [ ("class", json_string "File") ] in
      let fields =
        match f.location with
        | Some loc ->
            let loc =
              if String.starts_with ~prefix:"file:" loc then loc
              else "file://" ^ loc
            in
            fields @ [ ("location", json_string loc) ]
        | None -> fields
      in
      let fields =
        match f.path with
        | Some p -> fields @ [ ("path", json_string p) ]
        | None -> fields
      in
      let fields =
        match f.basename with
        | Some b -> fields @ [ ("basename", json_string b) ]
        | None -> fields
      in
      let fields =
        match f.nameroot with
        | Some n -> fields @ [ ("nameroot", json_string n) ]
        | None -> fields
      in
      let fields =
        match f.nameext with
        | Some n -> fields @ [ ("nameext", json_string n) ]
        | None -> fields
      in
      let fields =
        match f.size with
        | Some n -> fields @ [ ("size", Int64.to_string n) ]
        | None -> fields
      in
      json_object fields
  | Vdir d ->
      let fields = [ ("class", json_string "Directory") ] in
      let fields =
        match d.location with
        | Some loc ->
            let loc =
              if String.starts_with ~prefix:"file:" loc then loc
              else "file://" ^ loc
            in
            fields @ [ ("location", json_string loc) ]
        | None -> fields
      in
      let fields =
        match d.path with
        | Some p -> fields @ [ ("path", json_string p) ]
        | None -> fields
      in
      json_object fields
  | Varray xs -> "[" ^ String.concat "," (List.map to_json xs) ^ "]"
  | Vrecord kvs -> json_object (List.map (fun (k, v) -> (k, to_json v)) kvs)

let object_to_json obj = to_json (Vrecord obj)
