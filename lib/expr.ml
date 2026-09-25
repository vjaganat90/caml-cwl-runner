(** Parameter references ([$(inputs…)], [$(self…)], [$(runtime…)]) and string
    interpolation. A sole reference keeps its type. Inline JavaScript is a
    separate [ENGINE] of the same signature. Pure: no filesystem. *)

include Data.Expr

let default_runtime =
  {
    outdir = "/unused-outdir";
    tmpdir = "/unused-tmpdir";
    cores = 1.;
    ram = 256.;
  }

let runtime_with_cores cores = { default_runtime with cores }

let runtime_with ~outdir ~tmpdir ~cores =
  { outdir; tmpdir; cores; ram = default_runtime.ram }

let expr_err message = Error (Error.Expr { message })
let unsupported feature = Error (Error.Unsupported { feature })

let is_ident_start = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_ident_cont = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
  | _ -> false

type segment = Dot of string | Quote of string | Index of int

let rec ident_end s i =
  if i < String.length s && is_ident_cont s.[i] then ident_end s (i + 1) else i

let parse_ident s i =
  if i >= String.length s || not (is_ident_start s.[i]) then None
  else
    let j = ident_end s (i + 1) in
    Some (String.sub s i (j - i), j)

let rec skip_ws s i =
  if i < String.length s && (s.[i] = ' ' || s.[i] = '\t') then skip_ws s (i + 1)
  else i

let rec digits_end s i =
  if i < String.length s && s.[i] >= '0' && s.[i] <= '9' then
    digits_end s (i + 1)
  else i

let parse_param_ref s =
  let n = String.length s in
  if n < 3 || s.[0] <> '$' || s.[1] <> '(' || s.[n - 1] <> ')' then None
  else
    let body = String.sub s 2 (n - 3) in
    let i = skip_ws body 0 in
    match parse_ident body i with
    | None -> None
    | Some (root, i) ->
        let rec segs i acc =
          let i = skip_ws body i in
          if i >= String.length body then Some (root, List.rev acc)
          else if body.[i] = '.' then
            match parse_ident body (i + 1) with
            | None -> None
            | Some (name, j) -> segs j (Dot name :: acc)
          else if
            body.[i] = '['
            && i + 1 < String.length body
            && (body.[i + 1] = '\'' || body.[i + 1] = '"')
          then
            let q = body.[i + 1] in
            match String.index_from_opt body (i + 2) q with
            | None -> None
            | Some k ->
                if k + 1 < String.length body && body.[k + 1] = ']' then
                  let name = String.sub body (i + 2) (k - (i + 2)) in
                  segs (k + 2) (Quote name :: acc)
                else None
          else if body.[i] = '[' then
            let j = digits_end body (i + 1) in
            if j < String.length body && body.[j] = ']' && j > i + 1 then
              let idx = int_of_string (String.sub body (i + 1) (j - i - 1)) in
              segs (j + 1) (Index idx :: acc)
            else None
          else if body.[i] = '\'' || body.[i] = '"' then
            let q = body.[i] in
            match String.index_from_opt body (i + 1) q with
            | None -> None
            | Some k ->
                let name = String.sub body (i + 1) (k - i - 1) in
                segs (k + 1) (Quote name :: acc)
          else None
        in
        segs i []

let or_null f = function Some x -> Some (f x) | None -> Some Ty.Vnull

let file_field (f : Ty.file) = function
  | "location" -> or_null (fun s -> Ty.Vstring s) f.location
  | "path" -> or_null (fun s -> Ty.Vstring s) f.path
  | "basename" -> or_null (fun s -> Ty.Vstring s) f.basename
  | "nameroot" -> or_null (fun s -> Ty.Vstring s) f.nameroot
  | "nameext" -> or_null (fun s -> Ty.Vstring s) f.nameext
  | "checksum" -> or_null (fun s -> Ty.Vstring s) f.checksum
  | "size" -> or_null (fun n -> Ty.Vint n) f.size
  | "class" -> Some (Ty.Vstring "File")
  | _ -> None

let dir_field (d : Ty.directory) = function
  | "location" -> or_null (fun s -> Ty.Vstring s) d.location
  | "path" -> or_null (fun s -> Ty.Vstring s) d.path
  | "class" -> Some (Ty.Vstring "Directory")
  | _ -> None

let number_of_float f =
  if Float.is_integer f then Ty.Vint (Int64.of_float f) else Ty.Vfloat f

let eval_path ctx root segs =
  let start =
    match root with
    | "inputs" -> Ok (Ty.Vrecord ctx.inputs)
    | "self" -> Ok ctx.self
    | "runtime" ->
        Ok
          (Ty.Vrecord
             [
               ("outdir", Ty.Vstring ctx.runtime.outdir);
               ("tmpdir", Ty.Vstring ctx.runtime.tmpdir);
               ("cores", number_of_float ctx.runtime.cores);
               ("ram", number_of_float ctx.runtime.ram);
             ])
    | "null" -> Ok Ty.Vnull
    | other -> expr_err (Printf.sprintf "unknown parameter context '%s'" other)
  in
  let rec step current = function
    | [] -> Ok current
    | seg :: rest -> (
        let key =
          match seg with Dot k | Quote k -> k | Index i -> string_of_int i
        in
        match (current, seg) with
        | Ty.Vnull, _ ->
            expr_err (Printf.sprintf "cannot look up '%s' on null" key)
        | Ty.Vrecord kvs, (Dot k | Quote k) -> (
            match List.assoc_opt k kvs with
            | Some v -> step v rest
            | None -> expr_err (Printf.sprintf "missing field '%s' in object" k)
            )
        | Ty.Vfile f, (Dot k | Quote k) -> (
            match file_field f k with
            | Some v -> step v rest
            | None -> expr_err (Printf.sprintf "unknown File field '%s'" k))
        | Ty.Vdir d, (Dot k | Quote k) -> (
            match dir_field d k with
            | Some v -> step v rest
            | None -> expr_err (Printf.sprintf "unknown Directory field '%s'" k)
            )
        | Ty.Varray xs, (Dot "length" | Quote "length") when rest = [] ->
            Ok (Ty.Vint (Int64.of_int (List.length xs)))
        | Ty.Varray xs, Index i -> (
            match List.nth_opt xs i with
            | Some v -> step v rest
            | None -> expr_err (Printf.sprintf "array index %d out of range" i))
        | Ty.Vstring s, Index i ->
            if i >= 0 && i < String.length s then
              step (Ty.Vstring (String.make 1 s.[i])) rest
            else expr_err (Printf.sprintf "string index %d out of range" i)
        | _ ->
            expr_err
              (Printf.sprintf "cannot look up '%s' on a %s" key
                 (Ty.value_kind current)))
  in
  match start with Error _ as e -> e | Ok current -> step current segs

let text_of = function Ty.Vstring s -> s | v -> Ty.to_json_sorted v

let rec close_at s i depth open_c close_c quote =
  if i >= String.length s then None
  else
    let c = s.[i] in
    match quote with
    | Some _ when c = '\\' && i + 1 < String.length s ->
        close_at s (i + 2) depth open_c close_c quote
    | Some q when c = q -> close_at s (i + 1) depth open_c close_c None
    | Some _ -> close_at s (i + 1) depth open_c close_c quote
    | None when c = '\'' || c = '"' ->
        close_at s (i + 1) depth open_c close_c (Some c)
    | None when c = open_c -> close_at s (i + 1) (depth + 1) open_c close_c None
    | None when c = close_c ->
        if depth = 0 then Some i
        else close_at s (i + 1) (depth - 1) open_c close_c None
    | None -> close_at s (i + 1) depth open_c close_c None

let rec scan ctx s i buf =
  let n = String.length s in
  if i >= n then Ok (Buffer.contents buf)
  else if i + 1 < n && s.[i] = '\\' && s.[i + 1] = '\\' then (
    Buffer.add_char buf '\\';
    scan ctx s (i + 2) buf)
  else if
    i + 2 < n
    && s.[i] = '\\'
    && s.[i + 1] = '$'
    && (s.[i + 2] = '(' || s.[i + 2] = '{')
  then (
    Buffer.add_char buf '$';
    Buffer.add_char buf s.[i + 2];
    scan ctx s (i + 3) buf)
  else if i + 1 < n && s.[i] = '$' && (s.[i + 1] = '(' || s.[i + 1] = '{') then
    let open_c = s.[i + 1] in
    let close_c = if open_c = '(' then ')' else '}' in
    match close_at s (i + 2) 0 open_c close_c None with
    | None -> expr_err "unclosed parameter reference"
    | Some _ when open_c = '{' -> unsupported "InlineJavascriptRequirement"
    | Some j -> (
        let raw = String.sub s i (j + 1 - i) in
        match parse_param_ref raw with
        | None -> unsupported "InlineJavascriptRequirement"
        | Some (root, segs) -> (
            match eval_path ctx root segs with
            | Error _ as e -> e
            | Ok v ->
                Buffer.add_string buf (text_of v);
                scan ctx s (j + 1) buf))
  else (
    Buffer.add_char buf s.[i];
    scan ctx s (i + 1) buf)

module Param_ref : ENGINE = struct
  let eval ctx expr =
    let t = String.trim expr in
    match parse_param_ref t with
    | Some (root, segs) -> eval_path ctx root segs
    | None ->
        if String.exists (fun c -> c = '$' || c = '\\') expr then
          match scan ctx expr 0 (Buffer.create (String.length expr)) with
          | Error _ as e -> e
          | Ok text -> Ok (Ty.Vstring text)
        else Ok (Ty.Vstring expr)
end
