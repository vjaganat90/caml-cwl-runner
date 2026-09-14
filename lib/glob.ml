module type FS = sig
  val exists : string -> bool
  val is_dir : string -> bool
  val read_dir : string -> (string list, Error.t) result
  val realpath : string -> (string, Error.t) result
end

let ( let* ) = Error.( let* )

let join a b =
  if a = "" || a = "." then b
  else if b = "" || b = "." then a
  else if String.ends_with ~suffix:"/" a then a ^ b
  else a ^ "/" ^ b

let split_pattern s =
  let buf = Buffer.create (String.length s) in
  let rec go i acc escaped =
    if i >= String.length s then
      let last = Buffer.contents buf in
      List.rev (last :: acc)
    else
      let c = s.[i] in
      if escaped then (
        Buffer.add_char buf c;
        go (i + 1) acc false)
      else if c = '\\' then go (i + 1) acc true
      else if c = '/' then (
        let part = Buffer.contents buf in
        Buffer.clear buf;
        go (i + 1) (part :: acc) false)
      else (
        Buffer.add_char buf c;
        go (i + 1) acc false)
  in
  List.filter (fun p -> p <> "") (go 0 [] false)

let is_literal s =
  let rec go i escaped =
    if i >= String.length s then true
    else
      let c = s.[i] in
      if escaped then go (i + 1) false
      else if c = '\\' then go (i + 1) true
      else if c = '*' || c = '?' || c = '[' then false
      else go (i + 1) false
  in
  go 0 false

let compile_component s =
  match
    Re.Glob.glob_result ~anchored:true ~pathname:true ~period:true
      ~double_asterisk:true ~expand_braces:false s
  with
  | Error `Parse_error ->
      Error
        (Error.Runtime
           { message = Printf.sprintf "invalid glob pattern '%s'" s })
  | Ok re -> Ok (Re.compile re)

let trim_slash s =
  let n = String.length s in
  if n > 1 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

let strip_prefix ~prefix s =
  let prefix = trim_slash prefix in
  if s = prefix then Some ""
  else
    let p = prefix ^ "/" in
    if String.starts_with ~prefix:p s then
      Some (String.sub s (String.length p) (String.length s - String.length p))
    else None

let relativize ~root pattern =
  let root = trim_slash root in
  let pattern = String.trim pattern in
  if pattern = "" || pattern = "." then Ok []
  else if String.starts_with ~prefix:"/" pattern then
    match strip_prefix ~prefix:root pattern with
    | Some rest -> Ok (split_pattern rest)
    | None ->
        Error
          (Error.Runtime { message = "glob patterns must not start with '/'" })
  else Ok (split_pattern pattern)

let sort_unique xs =
  let xs = List.sort String.compare xs in
  let rec dedup = function
    | a :: (b :: _ as rest) when a = b -> dedup rest
    | a :: rest -> a :: dedup rest
    | [] -> []
  in
  dedup xs

let max_glob_depth = 64

let under ~root path =
  let root = trim_slash root in
  let path = trim_slash path in
  path = root || String.starts_with ~prefix:(root ^ "/") path

let in_roots ~roots path = List.exists (fun root -> under ~root path) roots

let may_list (module FS : FS) ~roots dir =
  match FS.realpath dir with
  | Error _ -> Ok false
  | Ok rp -> Ok (in_roots ~roots rp)

let rec collect (module FS : FS) ~roots ~depth dir parts =
  if depth > max_glob_depth then
    Error (Error.Runtime { message = "glob exceeded directory depth" })
  else
    match parts with
    | [] -> if FS.exists dir then Ok [ dir ] else Ok []
    | ".." :: _ ->
        Error (Error.Runtime { message = "glob pattern must not contain '..'" })
    | "**" :: rest ->
        let* zero = collect (module FS : FS) ~roots ~depth dir rest in
        if not (FS.is_dir dir) then Ok zero
        else
          let* list = may_list (module FS : FS) ~roots dir in
          if not list then Ok zero
          else
            let* names = FS.read_dir dir in
            let names =
              List.filter (fun n -> n <> "." && n <> ".." && n <> "") names
            in
            let* nested =
              Error.map_list
                (fun name ->
                  collect
                    (module FS : FS)
                    ~roots ~depth:(depth + 1) (join dir name) parts)
                names
            in
            Ok (zero @ List.concat nested)
    | comp :: rest ->
        if not (FS.is_dir dir) then Ok []
        else
          let* list = may_list (module FS : FS) ~roots dir in
          if not list then Ok []
          else if is_literal comp then
            let p = join dir comp in
            collect (module FS : FS) ~roots ~depth:(depth + 1) p rest
          else
            let* re = compile_component comp in
            let* names = FS.read_dir dir in
            let matched =
              List.filter
                (fun n -> n <> "." && n <> ".." && n <> "" && Re.execp re n)
                names
            in
            let* groups =
              Error.map_list
                (fun name ->
                  collect
                    (module FS : FS)
                    ~roots ~depth:(depth + 1) (join dir name) rest)
                matched
            in
            Ok (List.concat groups)

let glob (module FS : FS) ~root ~pattern ?roots () =
  let root = trim_slash root in
  let roots =
    match roots with
    | None | Some [] -> [ root ]
    | Some rs -> List.map trim_slash rs
  in
  let roots =
    List.map
      (fun r -> match FS.realpath r with Ok p -> trim_slash p | Error _ -> r)
      roots
  in
  let* parts = relativize ~root pattern in
  if List.exists (fun p -> p = "..") parts then
    Error (Error.Runtime { message = "glob pattern must not contain '..'" })
  else
    let* hits = collect (module FS : FS) ~roots ~depth:0 root parts in
    Ok (sort_unique hits)
