(** Nested YAML/JSON file contents, before CWL types. The only file reader.
    [load] resolves local [$import] and [$include]. Does not leak [Yaml.value].
    Default [FILE] is [In_channel]. Not [Document] (CommandLineTool | Workflow)
    and not [Type.value]. *)

include Data.Untyped_tree

let rec of_yaml : Yaml.value -> value = function
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

module Sys_file : FILE = struct
  let read path =
    try Ok (In_channel.with_open_text path In_channel.input_all) with
    | Sys_error msg -> Error (Error.Parse { path = Some path; message = msg })
    | exn ->
        Error
          (Error.Parse { path = Some path; message = Printexc.to_string exn })
end

let load_string ?path s = of_yaml_string ?path s
let assoc key = function Object kvs -> List.assoc_opt key kvs | _ -> None
let object_fields = function Object kvs -> Some kvs | _ -> None
let as_string = function String s -> Some s | _ -> None
let as_bool = function Bool b -> Some b | _ -> None
let as_int = function Int n -> Some n | _ -> None

let as_float = function
  | Float f -> Some f
  | Int n -> Some (Int64.to_float n)
  | _ -> None

let as_list = function Array xs -> Some xs | _ -> None
let is_null = function Null -> true | _ -> false

let string_field kvs key =
  match List.assoc_opt key kvs with Some (String s) -> Some s | _ -> None

let bool_field kvs key =
  match List.assoc_opt key kvs with Some (Bool b) -> Some b | _ -> None

let int_field kvs key =
  match List.assoc_opt key kvs with
  | Some (Int n) -> Some n
  | Some (Float f) when Float.is_integer f -> Some (Int64.of_float f)
  | _ -> None

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
  if is_remote uri then Error (Error.Unsupported { feature })
  else
    let uri = strip_file uri in
    let dir = base_dir base in
    if is_remote dir then Error (Error.Unsupported { feature })
    else if Filename.is_relative uri then
      Ok (normalize (Filename.concat dir uri))
    else Ok (normalize uri)

let schema path message = Error (Error.Schema { path; message })

let only_keys kvs allowed =
  match List.find_opt (fun (k, _) -> not (List.mem k allowed)) kvs with
  | None -> Ok ()
  | Some (k, _) -> schema k "unexpected field next to $import or $include"

let rec resolve (module F : FILE) ~base ~stack = function
  | Array xs ->
      let rec go acc = function
        | [] -> Ok (Array (List.rev acc))
        | x :: xs -> (
            match resolve (module F : FILE) ~base ~stack x with
            | Error _ as e -> e
            | Ok v -> go (v :: acc) xs)
      in
      go [] xs
  | Object kvs -> resolve_object (module F : FILE) ~base ~stack kvs
  | other -> Ok other

and resolve_object (module F : FILE) ~base ~stack kvs =
  let ( let* ) = Result.bind in
  let* base =
    match List.assoc_opt "$base" kvs with
    | None -> Ok base
    | Some (String raw) ->
        let* path = locate ~feature:"remote $base" ~base raw in
        if String.ends_with ~suffix:"/" (strip_file raw) then Ok (`Dir path)
        else Ok (`File path)
    | Some other ->
        schema "$base" (Format.asprintf "expected string, got %a" pp other)
  in
  match (List.assoc_opt "$import" kvs, List.assoc_opt "$include" kvs) with
  | Some _, Some _ -> schema "$import" "$import and $include are both set"
  | Some (String uri), None ->
      let* () = only_keys kvs [ "$import"; "$base" ] in
      splice (module F : FILE) ~base ~stack ~feature:"remote $import" uri
  | None, Some (String uri) -> (
      let* () = only_keys kvs [ "$include"; "$base" ] in
      let* path = locate ~feature:"remote $include" ~base uri in
      if List.mem path stack then schema path ("import cycle involving " ^ path)
      else
        match F.read path with Error _ as e -> e | Ok text -> Ok (String text))
  | Some _, None | None, Some _ ->
      schema "$import" "$import and $include must be strings"
  | None, None ->
      let rec go acc = function
        | [] -> Ok (Object (List.rev acc))
        | (k, v) :: rest -> (
            match resolve (module F : FILE) ~base ~stack v with
            | Error _ as e -> e
            | Ok v -> go ((k, v) :: acc) rest)
      in
      go [] kvs

and splice (module F : FILE) ~base ~stack ~feature uri =
  let ( let* ) = Result.bind in
  let* path = locate ~feature ~base uri in
  if List.mem path stack then schema path ("import cycle involving " ^ path)
  else
    match F.read path with
    | Error _ as e -> e
    | Ok text -> (
        match of_yaml_string ~path text with
        | Error _ as e -> e
        | Ok tree ->
            resolve
              (module F : FILE)
              ~base:(`File path) ~stack:(path :: stack) tree)

let load (module F : FILE) path =
  let ( let* ) = Result.bind in
  let* text = F.read path in
  let* tree = of_yaml_string ~path text in
  let path = normalize path in
  resolve (module F : FILE) ~base:(`File path) ~stack:[ path ] tree

let load_file path = load (module Sys_file) path
