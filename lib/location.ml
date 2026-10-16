let trim_slash s =
  let n = String.length s in
  (* cannot raise: n - 1 is at least 1 here *)
  if n > 1 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

let relative root path =
  let root = trim_slash root in
  let prefix = root ^ "/" in
  let n = String.length prefix in
  if path = root then Some ""
  else if String.starts_with ~prefix path then
    (* cannot raise: path starts with the n bytes of prefix *)
    Some (String.sub path n (String.length path - n))
  else None

let under root path = Option.is_some (relative root (trim_slash path))

let normalize path =
  let segs =
    String.split_on_char '/' path |> List.filter (fun s -> s <> "" && s <> ".")
  in
  let rec go acc = function
    | [] -> List.rev acc
    | ".." :: rest -> go (match acc with [] -> [] | _ :: tl -> tl) rest
    | seg :: rest -> go (seg :: acc) rest
  in
  let body = String.concat "/" (go [] segs) in
  if String.starts_with ~prefix:"/" path then "/" ^ body else body

(* RFC 3986, 2.1: [%] and two hex digits stand for that byte. A [%] without
   them is kept as it is. *)
let percent_decode s =
  let n = String.length s in
  let hex i =
    match if i < n then s.[i] else ' ' with
    | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
    | ('a' .. 'f' | 'A' .. 'F') as c ->
        Some (Char.code (Char.lowercase_ascii c) - Char.code 'a' + 10)
    | _ -> None
  in
  let buf = Buffer.create n in
  let rec go i =
    if i >= n then Buffer.contents buf
    else
      match (s.[i], hex (i + 1), hex (i + 2)) with
      | '%', Some h, Some l ->
          (* cannot raise: two hex digits make at most 255 *)
          Buffer.add_char buf (Char.chr ((16 * h) + l));
          go (i + 3)
      | c, _, _ ->
          Buffer.add_char buf c;
          go (i + 1)
  in
  go 0

let to_path s =
  let prefix = "file://" in
  percent_decode
    (if not (String.starts_with ~prefix s) then s
     else
       (* cannot raise: s starts with the 7 bytes of prefix *)
       let rest = String.sub s 7 (String.length s - 7) in
       match String.index_opt rest '/' with
       | None -> rest
       (* cannot raise: i is an index of rest *)
       | Some i -> String.sub rest i (String.length rest - i))

let to_uri loc =
  if String.starts_with ~prefix:"file:" loc then loc else "file://" ^ loc

let is_remote s =
  String.starts_with ~prefix:"http://" s
  || String.starts_with ~prefix:"https://" s

let locate ~feature dir name =
  if is_remote name || is_remote dir then Error.unsupported feature
  else
    let name = to_path name in
    Ok
      (normalize
         (if Filename.is_relative name then Filename.concat dir name else name))
