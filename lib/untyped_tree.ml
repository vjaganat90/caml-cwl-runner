include Data.Untyped_tree
open Error.Syntax

(* A plain scalar by the YAML 1.2 core schema: only [true], [True], [TRUE] and
   the [false] forms are booleans, an integer is exact, and one outside the
   [int64] range is a [Float]. *)
let resolve_plain s =
  let all_in ok from =
    from < String.length s && Seq.for_all ok (Seq.drop from (String.to_seq s))
  in
  let starts prefix = String.starts_with ~prefix s in
  let sign = if starts "-" || starts "+" then 1 else 0 in
  let is_int =
    if starts "0x" then
      all_in
        (function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false)
        2
    else if starts "0o" then
      all_in (function '0' .. '7' -> true | _ -> false) 2
    else all_in (function '0' .. '9' -> true | _ -> false) sign
  in
  let is_float =
    all_in
      (function '0' .. '9' | '.' | '+' | '-' | 'e' | 'E' -> true | _ -> false)
      0
  in
  match s with
  | "" | "~" | "null" | "Null" | "NULL" -> Null
  | "true" | "True" | "TRUE" -> Bool true
  | "false" | "False" | "FALSE" -> Bool false
  | ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" -> Float infinity
  | "-.inf" | "-.Inf" | "-.INF" -> Float neg_infinity
  | ".nan" | ".NaN" | ".NAN" -> Float nan
  | _ -> (
      match (Int64.of_string_opt s, float_of_string_opt s) with
      | Some n, _ when is_int && (n >= 0L || sign = 1) -> Int n
      | _, Some f when is_int || is_float -> Float f
      | _ -> String s)

let rec of_yaml ~path : Yaml.yaml -> (value, Error.t) result =
  let fail message = Error (Error.Parse { path; message }) in
  function
  | `Scalar { style = `Plain; value; _ } -> Ok (resolve_plain value)
  | `Scalar { value; _ } -> Ok (String value)
  | `Alias _ -> fail "aliases are not supported"
  | `A { s_members; _ } ->
      let* xs = Error.map_list (of_yaml ~path) s_members in
      Ok (Array xs)
  | `O { m_members; _ } ->
      let entry = function
        | `Scalar { Yaml.value = k; _ }, v ->
            let* v = of_yaml ~path v in
            Ok (k, v)
        | _ -> fail "a key is not a scalar"
      in
      let* kvs = Error.map_list entry m_members in
      Ok (Object kvs)

let of_yaml_string ?(path = "") s =
  let path = if path = "" then None else Some path in
  match Yaml.yaml_of_string s with
  | Ok y -> of_yaml ~path y
  | Error (`Msg message) -> Error (Error.Parse { path; message })

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
let int_field = field "int" (function Int n -> Some n | _ -> None)

let base_dir = function
  | `File path -> Filename.dirname path
  | `Dir path -> path

let locate ~feature ~base uri = Location.locate ~feature (base_dir base) uri

(* A [$base] that ends in [/] names a directory; any other names a file. *)
let base_of ~base raw =
  let+ path = locate ~feature:"remote $base" ~base raw in
  if String.ends_with ~suffix:"/" (Location.to_path raw) then `Dir path
  else `File path

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
      | Some (String raw) -> base_of ~base raw
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
  let path = Location.normalize path in
  resolve (module F) ~base:(`File path) ~stack:[ path ] tree
