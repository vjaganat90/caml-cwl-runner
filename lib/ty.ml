include Data.Type
open Error.Syntax

let default_binding =
  {
    position = Pos 0;
    prefix = None;
    separate = true;
    item_separator = None;
    value_from = None;
  }

let rec is_optional = function
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
  | Any -> "Any"
  | Enum _ -> "enum"
  | Record _ -> "record"
  | Array { items; _ } -> type_name items ^ "[]"
  | Union ts -> "[" ^ String.concat ", " (List.map type_name ts) ^ "]"

(* The fields of a File or Directory that ccr reads as strings; a File's
   [size] is an int. *)
let string_fields = function
  | "File" ->
      [ "location"; "path"; "basename"; "nameroot"; "nameext"; "checksum" ]
  | _ -> [ "location"; "path" ]

let check_fields param cls kvs =
  let* _ =
    Error.map_list
      (Untyped_tree.string_field ~json_path:param kvs)
      (string_fields cls)
  in
  if cls = "File" then
    let+ _ = Untyped_tree.int_field ~json_path:param kvs "size" in
    ()
  else Ok ()

let class_of = function
  | Untyped_tree.Object kvs -> (
      match List.assoc_opt "class" kvs with
      | Some (Untyped_tree.String (("File" | "Directory") as cls))
        when Result.is_ok (check_fields "" cls kvs) ->
          Some cls
      | _ -> None)
  | Untyped_tree.(Null | Bool _ | Int _ | Float _ | String _ | Array _) -> None

let get key = function
  | Untyped_tree.Object kvs -> (
      match List.assoc_opt key kvs with
      | None | Some Untyped_tree.Null -> None
      | x -> x)
  | _ -> None

let get_string key v =
  match get key v with Some (Untyped_tree.String s) -> Some s | _ -> None

let set key x = function
  | Untyped_tree.Object kvs when List.mem_assoc key kvs ->
      Untyped_tree.Object
        (List.map (fun (k, y) -> (k, if k = key then x else y)) kvs)
  | Untyped_tree.Object kvs -> Untyped_tree.Object (kvs @ [ (key, x) ])
  | v -> v

let value_kind v =
  match v with
  | Untyped_tree.Null -> "null"
  | Untyped_tree.Bool _ -> "boolean"
  | Untyped_tree.Int _ -> "int"
  | Untyped_tree.Float _ -> "float"
  | Untyped_tree.String _ -> "string"
  | Untyped_tree.Array _ -> "array"
  | Untyped_tree.Object _ -> Option.value (class_of v) ~default:"record"

let or_else a b = match a with Some _ as x -> x | None -> b
let path v = or_else (get_string "path" v) (get_string "location" v)

(* The containers the walkers descend into: arrays and records. File and
   Directory stay leaves until secondaryFiles and listing are walked. visit
   returns a container's children and a function that rebuilds the container
   from new children. *)
let visit v =
  match v with
  | Untyped_tree.Array xs -> Some (xs, fun ys -> Untyped_tree.Array ys)
  | Untyped_tree.Object kvs when class_of v = None ->
      Some
        ( List.map snd kvs,
          fun ys ->
            (* cannot raise: the walk returns one child per key it was given *)
            Untyped_tree.Object (List.map2 (fun (k, _) y -> (k, y)) kvs ys) )
  | Untyped_tree.(Null | Bool _ | Int _ | Float _ | String _ | Object _) -> None

let rec map f v =
  match visit v with None -> f v | Some (xs, put) -> put (List.map (map f) xs)

let rec map_result f v =
  match visit v with
  | None -> f v
  | Some (xs, put) ->
      let+ xs = Error.map_list (map_result f) xs in
      put xs

let rec fold f acc v =
  match visit v with
  | None -> f acc v
  | Some (xs, _) -> List.fold_left (fold f) acc xs

let rec map_state f acc ys = function
  | [] -> Ok (List.rev ys, acc)
  | x :: xs ->
      let* y, acc = f acc x in
      map_state f acc (y :: ys) xs

let rec fold_map f acc v =
  match visit v with
  | None -> f acc v
  | Some (xs, put) ->
      let+ ys, acc = map_state (fun acc x -> fold_map f acc x) acc [] xs in
      (put ys, acc)

let file_basename_of location_or_path =
  match location_or_path with
  | None -> None
  | Some p ->
      let base = Filename.basename (Location.to_path p) in
      if base = "" then None else Some base

let split_name basename =
  match String.rindex_opt basename '.' with
  | None | Some 0 -> (basename, "")
  | Some i ->
      ( String.sub basename 0 i,
        String.sub basename i (String.length basename - i) )

(* [fill v] sets the fields of the File or Directory [v] that the spec has an
   implementation compute, where [v] leaves them unset. *)
let fill v =
  let default key x v =
    match x with
    | Some s when get_string key v = None -> set key (Untyped_tree.String s) v
    | _ -> v
  in
  let v = default "path" (file_basename_of (get_string "location" v)) v in
  let v =
    default "basename" (Option.map Filename.basename (get_string "path" v)) v
  in
  match (class_of v, get_string "basename" v) with
  | Some "File", Some b ->
      let nameroot, nameext = split_name b in
      default "nameroot" (Some nameroot) v |> default "nameext" (Some nameext)
  | _ -> v

let fill_file_paths = map (fun v -> if class_of v = None then v else fill v)

(* The value of field [f] in the record [v]; a missing field is null. *)
let field_value (f : field) v =
  Option.value (get f.name v) ~default:Untyped_tree.Null

let rec matches ty v =
  match (ty, v) with
  | Null, Untyped_tree.Null -> true
  | Boolean, Untyped_tree.Bool _ -> true
  | (Int | Long), Untyped_tree.Int _ -> true
  | (Float | Double), Untyped_tree.(Float _ | Int _) -> true
  | String, Untyped_tree.String _ -> true
  | Enum symbols, Untyped_tree.String s -> List.mem s symbols
  | File, v -> class_of v = Some "File"
  | Directory, v -> class_of v = Some "Directory"
  | Array { items; _ }, Untyped_tree.Array xs -> List.for_all (matches items) xs
  | Union ts, v -> List.exists (fun t -> matches t v) ts
  | Any, Untyped_tree.Null -> false
  | Any, _ -> true
  | Record fields, Untyped_tree.Object _ when class_of v = None ->
      List.for_all (fun (f : field) -> matches f.ty (field_value f v)) fields
  | _ -> false

let require_class param expected kvs =
  match List.assoc_opt "class" kvs with
  | Some (Untyped_tree.String s) when s = expected -> Ok ()
  | None -> Ok ()
  | Some v ->
      Error
        (Error.Type
           { param; expected; got = Format.asprintf "%a" Untyped_tree.pp v })

let read_object param cls kvs =
  let* () = require_class param cls kvs in
  let+ () = check_fields param cls kvs in
  fill (set "class" (Untyped_tree.String cls) (Untyped_tree.Object kvs))

let rec coerce param ty doc =
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
            match coerce param t doc with
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
  | Null, Untyped_tree.Null
  | Boolean, Untyped_tree.Bool _
  | (Int | Long), Untyped_tree.Int _
  | (Float | Double), Untyped_tree.(Float _ | Int _)
  | String, Untyped_tree.String _ ->
      Ok doc
  | Any, Untyped_tree.(Bool _ | Int _ | Float _ | String _ | Array _ | Object _)
    ->
      Ok doc
  | Enum symbols, Untyped_tree.String s when List.mem s symbols -> Ok doc
  | String, Untyped_tree.Int n -> Ok (Untyped_tree.String (Int64.to_string n))
  | String, Untyped_tree.Float f -> Ok (Untyped_tree.String (string_of_float f))
  | File, Untyped_tree.Object kvs -> read_object param "File" kvs
  | Directory, Untyped_tree.Object kvs -> read_object param "Directory" kvs
  | Array { items; _ }, Untyped_tree.Array xs ->
      let+ xs = Error.map_list (coerce param items) xs in
      Untyped_tree.Array xs
  | Record fields, Untyped_tree.Object kvs when class_of doc = None ->
      let field (f : field) =
        let+ v = coerce (param ^ "." ^ f.name) f.ty (field_value f doc) in
        (f.name, v)
      in
      let+ coerced = Error.map_list field fields in
      let keep (k, v) =
        (k, Option.value (List.assoc_opt k coerced) ~default:v)
      in
      Untyped_tree.Object (List.map keep kvs)
  | _, Untyped_tree.Null ->
      if is_optional ty then Ok Untyped_tree.Null else fail (type_name ty)
  | _ -> fail (type_name ty)

let object_of_tree doc =
  match doc with
  | Untyped_tree.Null -> Ok []
  | Untyped_tree.Object kvs ->
      Ok (List.map (fun (k, v) -> (k, fill_file_paths v)) kvs)
  | Untyped_tree.(Bool _ | Int _ | Float _ | String _ | Array _) ->
      Error
        (Error.Schema
           {
             path = "/";
             message =
               Printf.sprintf "job must be a YAML/JSON object, got %s"
                 (Format.asprintf "%a" Untyped_tree.pp doc);
           })

(* Process.yml, InputFormat: the formats "allowed as input to this
   parameter". No ontology is read, so they are "tested by exact match", and
   a File with no [format] has none of them. A field's formats apply to its
   own value. *)
let rec check_format param ty formats v =
  let allowed f =
    class_of f <> Some "File"
    || Option.fold ~none:false
         ~some:(fun x -> List.mem x formats)
         (get_string "format" f)
  in
  let fail f =
    let got = Option.value (get_string "format" f) ~default:"no format" in
    Error (Error.Type { param; expected = String.concat " or " formats; got })
  in
  let* () =
    match fold (fun bad f -> if allowed f then bad else f :: bad) [] v with
    | f :: _ when formats <> [] -> fail f
    | _ -> Ok ()
  in
  let nested t x = check_format param t [] x in
  match (ty, v) with
  | Record fields, Untyped_tree.Object _ ->
      Error.map_list
        (fun (f : field) ->
          check_format (param ^ "." ^ f.name) f.ty f.format (field_value f v))
        fields
      |> Result.map ignore
  | Array { items; _ }, Untyped_tree.Array xs ->
      Result.map ignore (Error.map_list (nested items) xs)
  | Union ts, _ -> (
      match List.find_opt (fun t -> matches t v) ts with
      | Some t -> nested t v
      | None -> Ok ())
  | _ -> Ok ()

let apply_defaults_and_check inputs job =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | spec :: rest -> (
        let provided = List.assoc_opt spec.id job in
        let raw =
          match provided with
          | Some Untyped_tree.Null | None -> (
              match spec.default with
              | Some d -> Ok (Some (fill_file_paths d))
              | None when is_optional spec.ty -> Ok (Some Untyped_tree.Null)
              | None -> Error (Error.Missing { param = spec.id }))
          | Some v -> Ok (Some v)
        in
        match raw with
        | Error _ as e -> e
        | Ok None -> go acc rest
        | Ok (Some v) ->
            if
              matches spec.ty v
              ||
              match v with
              | Untyped_tree.Null -> is_optional spec.ty
              | _ -> false
            then
              let* () = check_format spec.id spec.ty spec.format v in
              go ((spec.id, fill_file_paths v) :: acc) rest
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

let rec string_of_value v =
  match v with
  | Untyped_tree.Null -> "null"
  | Untyped_tree.Bool b -> string_of_bool b
  | Untyped_tree.Int n -> Int64.to_string n
  | Untyped_tree.Float f ->
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
  | Untyped_tree.String s -> s
  | Untyped_tree.Array xs ->
      "[" ^ String.concat ", " (List.map string_of_value xs) ^ "]"
  | Untyped_tree.Object kvs when class_of v = None ->
      "{"
      ^ String.concat ", "
          (List.map (fun (k, v) -> k ^ ": " ^ string_of_value v) kvs)
      ^ "}"
  | Untyped_tree.Object _ -> Option.value (path v) ~default:""

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

let by_key fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let rec to_json ~sorted v =
  match v with
  | Untyped_tree.Null -> "null"
  | Untyped_tree.Bool b -> string_of_bool b
  | Untyped_tree.Int n -> Int64.to_string n
  | Untyped_tree.Float f ->
      if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f
      else string_of_float f
  | Untyped_tree.String s -> json_string s
  | Untyped_tree.Array xs ->
      "[" ^ String.concat "," (List.map (to_json ~sorted) xs) ^ "]"
  | Untyped_tree.Object kvs ->
      let field (k, x) =
        json_string k ^ ":"
        ^
        match x with
        | Untyped_tree.String loc when k = "location" && class_of v <> None ->
            json_string (Location.to_uri loc)
        | x -> to_json ~sorted x
      in
      let kvs = if sorted then by_key kvs else kvs in
      "{" ^ String.concat "," (List.map field kvs) ^ "}"
