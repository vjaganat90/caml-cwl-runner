include Data.Doc

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

let load (module F : FILE) ~path =
  match F.read path with Error _ as e -> e | Ok s -> of_yaml_string ~path s

let load_file path = load (module Sys_file) ~path
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
