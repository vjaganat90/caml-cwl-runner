include Data.Untyped_tree
open Error.Syntax

let rec of_yaml = function
  | `Null -> Null
  | `Bool b -> Bool b
  | `Float f ->
      if
        Float.is_integer f
        && f >= Int64.to_float Int64.min_int
        && f <= Int64.to_float Int64.max_int
      then Int (Int64.of_float f)
      else Float f
  | `String s -> String s
  | `A xs -> Array (List.map of_yaml xs)
  | `O kvs -> Object (List.map (fun (k, v) -> (k, of_yaml v)) kvs)

let of_yaml_string ?(path = "") s =
  match Yaml.of_string s with
  | Ok v -> Ok (of_yaml v)
  | Error (`Msg msg) ->
      let path = if path = "" then None else Some path in
      Error (Error.Parse { path; message = msg })

let rec pp fmt = function
  | Null -> Format.pp_print_string fmt "null"
  | Bool b -> Format.pp_print_bool fmt b
  | Int n -> Format.fprintf fmt "%Ld" n
  | Float f -> Format.pp_print_float fmt f
  | String s -> Format.fprintf fmt "%S" s
  | Array xs ->
      Format.fprintf fmt "[%a]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           pp)
        xs
  | Object kvs ->
      Format.fprintf fmt "{%a}"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           (fun fmt (k, v) -> Format.fprintf fmt "%s: %a" k pp v))
        kvs

let child path key = if path = "" then key else path ^ "." ^ key
let expected what value = Format.asprintf "expected %s, got %a" what pp value

(* A missing key and [null] are both absent. A present value that [read] does
   not accept is a schema error at the key's path, never [None]. *)
let field what read ~json_path kvs key =
  match List.assoc_opt key kvs with
  | None | Some Null -> Ok None
  | Some v -> (
      match read v with
      | Some x -> Ok (Some x)
      | None -> Error.schema (child json_path key) (expected what v))

let string_field = field "string" (function String s -> Some s | _ -> None)
let bool_field = field "boolean" (function Bool b -> Some b | _ -> None)

let int_field =
  field "int" (function
    | Int n -> Some n
    | Float f when Float.is_integer f -> Some (Int64.of_float f)
    | _ -> None)

let is_remote s =
  String.starts_with ~prefix:"http://" s
  || String.starts_with ~prefix:"https://" s

let strip_file s =
  let prefix = "file://" in
  if not (String.starts_with ~prefix s) then s
  else
    let rest =
      String.sub s (String.length prefix)
        (String.length s - String.length prefix)
    in
    if rest <> "" && rest.[0] = '/' then rest
    else
      match String.index_opt rest '/' with
      | None -> rest
      | Some i -> String.sub rest i (String.length rest - i)

let normalize path =
  let rooted = path <> "" && path.[0] = '/' in
  let segs =
    String.split_on_char '/' path |> List.filter (fun s -> s <> "" && s <> ".")
  in
  let rec go acc = function
    | [] -> List.rev acc
    | ".." :: rest -> (
        match acc with [] -> go [] rest | _ :: tl -> go tl rest)
    | seg :: rest -> go (seg :: acc) rest
  in
  let body = String.concat "/" (go [] segs) in
  if rooted then "/" ^ body else body

let base_dir = function
  | `File path -> Filename.dirname path
  | `Dir path -> path

let locate ~feature ~base uri =
  if is_remote uri then Error.unsupported feature
  else
    let uri = strip_file uri in
    let dir = base_dir base in
    if is_remote dir then Error.unsupported feature
    else if Filename.is_relative uri then
      Ok (normalize (Filename.concat dir uri))
    else Ok (normalize uri)

let only_keys kvs allowed =
  match List.find_opt (fun (k, _) -> not (List.mem k allowed)) kvs with
  | None -> Ok ()
  | Some (k, _) -> Error.schema k "unexpected field next to $import or $include"

let resolve (module F : FILE) ~base ~stack tree =
  let rec resolve ~base ~stack = function
    | Array xs ->
        let rec go acc = function
          | [] -> Ok (Array (List.rev acc))
          | x :: xs -> (
              match resolve ~base ~stack x with
              | Error _ as e -> e
              | Ok v -> go (v :: acc) xs)
        in
        go [] xs
    | Object kvs -> resolve_object ~base ~stack kvs
    | other -> Ok other
  and resolve_object ~base ~stack kvs =
    let* base =
      match List.assoc_opt "$base" kvs with
      | None -> Ok base
      | Some (String raw) ->
          let* path = locate ~feature:"remote $base" ~base raw in
          if String.ends_with ~suffix:"/" (strip_file raw) then Ok (`Dir path)
          else Ok (`File path)
      | Some other -> Error.schema "$base" (expected "string" other)
    in
    match (List.assoc_opt "$import" kvs, List.assoc_opt "$include" kvs) with
    | Some _, Some _ ->
        Error.schema "$import" "$import and $include are both set"
    | Some (String uri), None ->
        let* () = only_keys kvs [ "$import"; "$base" ] in
        splice ~base ~stack ~feature:"remote $import" uri
    | None, Some (String uri) -> (
        let* () = only_keys kvs [ "$include"; "$base" ] in
        let* path = locate ~feature:"remote $include" ~base uri in
        if List.mem path stack then
          Error.schema path ("import cycle involving " ^ path)
        else
          match F.read_file path with
          | Error _ as e -> e
          | Ok text -> Ok (String text))
    | Some _, None | None, Some _ ->
        Error.schema "$import" "$import and $include must be strings"
    | None, None ->
        let rec go acc = function
          | [] -> Ok (Object (List.rev acc))
          | (k, v) :: rest -> (
              match resolve ~base ~stack v with
              | Error _ as e -> e
              | Ok v -> go ((k, v) :: acc) rest)
        in
        go [] kvs
  and splice ~base ~stack ~feature uri =
    let* path = locate ~feature ~base uri in
    if List.mem path stack then
      Error.schema path ("import cycle involving " ^ path)
    else
      match F.read_file path with
      | Error _ as e -> e
      | Ok text -> (
          match of_yaml_string ~path text with
          | Error _ as e -> e
          | Ok tree -> resolve ~base:(`File path) ~stack:(path :: stack) tree)
  in
  resolve ~base ~stack tree

let load (module F : FILE) path =
  let* text = F.read_file path in
  let* tree = of_yaml_string ~path text in
  let path = normalize path in
  resolve (module F) ~base:(`File path) ~stack:[ path ] tree
