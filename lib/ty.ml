(** Avro-ish CWL types and runtime values. Nested [inputBinding] on arrays lives
    here so [Command_line_tool] can depend on [Type] without a cycle. Walkers
    recurse into arrays and records; callers handle leaves. Does not load
    documents or spawn. *)

include Data.Type

let ( let* ) = Error.( let* )

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

let or_else a b = match a with Some _ as x -> x | None -> b
let file_loc (f : file) = or_else f.location f.path
let dir_loc (d : directory) = or_else d.location d.path
let file_path (f : file) = or_else f.path f.location
let dir_path (d : directory) = or_else d.path d.location

let rec map f = function
  | Varray xs -> Varray (List.map (map f) xs)
  | Vrecord kvs -> Vrecord (List.map (fun (k, v) -> (k, map f v)) kvs)
  | v -> f v

let rec map_result f = function
  | Varray xs ->
      let ( let* ) = Error.( let* ) in
      let* xs = Error.map_list (map_result f) xs in
      Ok (Varray xs)
  | Vrecord kvs ->
      let ( let* ) = Error.( let* ) in
      let* kvs =
        Error.map_list
          (fun (k, v) ->
            let* v = map_result f v in
            Ok (k, v))
          kvs
      in
      Ok (Vrecord kvs)
  | v -> f v

let rec fold f acc = function
  | Varray xs -> List.fold_left (fold f) acc xs
  | Vrecord kvs -> List.fold_left (fun acc (_, v) -> fold f acc v) acc kvs
  | v -> f acc v

let rec fold_map f acc = function
  | Varray xs ->
      let ( let* ) = Error.( let* ) in
      let rec go acc acc_xs = function
        | [] -> Ok (Varray (List.rev acc_xs), acc)
        | x :: xs ->
            let* x, acc = fold_map f acc x in
            go acc (x :: acc_xs) xs
      in
      go acc [] xs
  | Vrecord kvs ->
      let ( let* ) = Error.( let* ) in
      let rec go acc acc_kvs = function
        | [] -> Ok (Vrecord (List.rev acc_kvs), acc)
        | (k, v) :: rest ->
            let* v, acc = fold_map f acc v in
            go acc ((k, v) :: acc_kvs) rest
      in
      go acc [] kvs
  | v -> f acc v

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
  let path = or_else f.path (file_basename_of f.location) in
  let basename = or_else f.basename (Option.map Filename.basename path) in
  let nameroot, nameext =
    match basename with
    | None -> (f.nameroot, f.nameext)
    | Some b ->
        let nr, ne = split_name b in
        (or_else f.nameroot (Some nr), or_else f.nameext (Some ne))
  in
  { f with path; basename; nameroot; nameext }

let fill_file_paths =
  map (function
    | Vfile f -> Vfile (fill_file f)
    | Vdir d ->
        Vdir { d with path = or_else d.path (file_basename_of d.location) }
    | v -> v)

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

let require_class param expected kvs =
  match List.assoc_opt "class" kvs with
  | Some (Untyped_tree.String s) when s = expected -> Ok ()
  | None -> Ok ()
  | Some v ->
      Error
        (Error.Type
           { param; expected; got = Format.asprintf "%a" Untyped_tree.pp v })

let rec parse_file_object param kvs =
  let* () = require_class param "File" kvs in
  Ok
    (Vfile
       (fill_file
          {
            location = Untyped_tree.string_field kvs "location";
            path = Untyped_tree.string_field kvs "path";
            basename = Untyped_tree.string_field kvs "basename";
            nameroot = Untyped_tree.string_field kvs "nameroot";
            nameext = Untyped_tree.string_field kvs "nameext";
            checksum = Untyped_tree.string_field kvs "checksum";
            size = Untyped_tree.int_field kvs "size";
          }))

and parse_directory_object param kvs =
  let* () = require_class param "Directory" kvs in
  Ok
    (fill_file_paths
       (Vdir
          {
            location = Untyped_tree.string_field kvs "location";
            path = Untyped_tree.string_field kvs "path";
          }))

and value_of_tree param ty doc =
  let fail expected =
    Error
      (Error.Type
         { param; expected; got = Format.asprintf "%a" Untyped_tree.pp doc })
  in
  match (ty, doc) with
  | Union ts, doc ->
      let rec try_ts = function
        | [] -> fail (type_name ty)
        | t :: rest -> (
            match value_of_tree param t doc with
            | Ok v -> Ok v
            | Error _ -> try_ts rest)
      in
      (* Prefer a non-null match when the document is not null. *)
      let ordered =
        match doc with
        | Untyped_tree.Null ->
            List.filter (function Null -> true | _ -> false) ts
            @ List.filter (function Null -> false | _ -> true) ts
        | _ ->
            List.filter (function Null -> false | _ -> true) ts
            @ List.filter (function Null -> true | _ -> false) ts
      in
      try_ts ordered
  | Null, Untyped_tree.Null -> Ok Vnull
  | Boolean, Untyped_tree.Bool b -> Ok (Vbool b)
  | (Int | Long), Untyped_tree.Int n -> Ok (Vint n)
  | (Int | Long), Untyped_tree.Float f when Float.is_integer f ->
      Ok (Vint (Int64.of_float f))
  | (Float | Double), Untyped_tree.Float f -> Ok (Vfloat f)
  | (Float | Double), Untyped_tree.Int n -> Ok (Vfloat (Int64.to_float n))
  | String, Untyped_tree.String s -> Ok (Vstring s)
  | String, Untyped_tree.Int n -> Ok (Vstring (Int64.to_string n))
  | String, Untyped_tree.Float f -> Ok (Vstring (string_of_float f))
  | File, Untyped_tree.Object kvs -> parse_file_object param kvs
  | Directory, Untyped_tree.Object kvs -> parse_directory_object param kvs
  | Array { items; _ }, Untyped_tree.Array xs ->
      let* xs = Error.map_list (value_of_tree param items) xs in
      Ok (Varray xs)
  | _, Untyped_tree.Null ->
      if is_optional ty then Ok Vnull else fail (type_name ty)
  | _ -> fail (type_name ty)

let object_of_tree = function
  | Untyped_tree.Null -> Ok []
  | Untyped_tree.Object kvs ->
      (* Loose decode: files/dirs recognized by class, else generic. *)
      let rec of_any = function
        | Untyped_tree.Null -> Vnull
        | Untyped_tree.Bool b -> Vbool b
        | Untyped_tree.Int n -> Vint n
        | Untyped_tree.Float f -> Vfloat f
        | Untyped_tree.String s -> Vstring s
        | Untyped_tree.Array xs -> Varray (List.map of_any xs)
        | Untyped_tree.Object kvs -> (
            match List.assoc_opt "class" kvs with
            | Some (Untyped_tree.String "File") -> (
                match parse_file_object "job" kvs with
                | Ok v -> v
                | Error _ ->
                    Vrecord (List.map (fun (k, v) -> (k, of_any v)) kvs))
            | Some (Untyped_tree.String "Directory") -> (
                match parse_directory_object "job" kvs with
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
                 (Format.asprintf "%a" Untyped_tree.pp other);
           })

let apply_defaults_and_check inputs job =
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
  | Vfile f -> Option.value (file_path f) ~default:""
  | Vdir d -> Option.value (dir_path d) ~default:""
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

let add_opt name conv v fields =
  match v with None -> fields | Some x -> fields @ [ (name, conv x) ]

let file_uri loc =
  if String.starts_with ~prefix:"file:" loc then loc else "file://" ^ loc

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
      [ ("class", json_string "File") ]
      |> add_opt "location" (fun loc -> json_string (file_uri loc)) f.location
      |> add_opt "path" json_string f.path
      |> add_opt "basename" json_string f.basename
      |> add_opt "nameroot" json_string f.nameroot
      |> add_opt "nameext" json_string f.nameext
      |> add_opt "size" Int64.to_string f.size
      |> json_object
  | Vdir d ->
      [ ("class", json_string "Directory") ]
      |> add_opt "location" (fun loc -> json_string (file_uri loc)) d.location
      |> add_opt "path" json_string d.path
      |> json_object
  | Varray xs -> "[" ^ String.concat "," (List.map to_json xs) ^ "]"
  | Vrecord kvs -> json_object (List.map (fun (k, v) -> (k, to_json v)) kvs)

let object_to_json obj = to_json (Vrecord obj)
