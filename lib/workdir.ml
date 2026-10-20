open Error.Syntax

let safe_leaf ~what s =
  let bad =
    s = "" || s = "." || s = ".."
    || String.exists (function '/' | '\\' | '\000' -> true | _ -> false) s
  in
  if bad then
    Error.runtime
      (Printf.sprintf "%s must be a single path segment, got %S" what s)
  else Ok s

(* Only a [file] URI, or a reference with no scheme, names a local file. *)
let local_path s =
  let scheme =
    match String.index_opt s ':' with
    | Some i
      when i > 0
           && String.length s > i + 2
           && s.[i + 1] = '/'
           && s.[i + 2] = '/' ->
        (* cannot raise: i is an index of s *)
        String.lowercase_ascii (String.sub s 0 i)
    | _ -> "file"
  in
  if scheme = "file" then Ok (Location.to_path s)
  else Error.runtime (Printf.sprintf "unsupported location scheme %S" scheme)

(* A [location] is a URI reference and a [path] a plain path. *)
let source ~dir ~what location path =
  let+ src =
    match (location, path) with
    | Some loc, _ -> local_path loc
    | None, Some path -> Ok path
    | None, None ->
        Error.runtime (Printf.sprintf "%s input missing location" what)
  in
  if Filename.is_relative src then Filename.concat dir src else src

(* [v] with its [path] and [location] set to [path] and [location], each
   where it is [Some]. A leaf that is not a File or Directory has neither. *)
let set_paths v (path, location) =
  let set key x v =
    Option.fold ~none:v ~some:(fun s -> Ty.set key (Untyped_tree.String s) v) x
  in
  set "path" path v |> set "location" location

let locate dir v =
  Ty.map_result
    (fun v ->
      match Ty.class_of v with
      | None -> Ok v
      | Some what ->
          let+ src =
            source ~dir ~what
              (Ty.get_string "location" v)
              (Ty.get_string "path" v)
          in
          let path = if what = "File" then None else Some src in
          set_paths v (path, Some src))
    v

let stage_leaf (module R : Io.WRITE) ~container ~outdir used v =
  match (Ty.class_of v, Ty.get_string "location" v) with
  | Some "File", Some src ->
      let raw_base =
        Option.value
          (Ty.get_string "basename" v)
          ~default:(Filename.basename src)
      in
      let* base = safe_leaf ~what:"File basename" raw_base in
      if List.mem base used then
        Error.runtime (Printf.sprintf "staging collision on basename '%s'" base)
      else
        let dst = Filename.concat outdir base in
        let* () = R.copy_file src dst in
        let seen = Location.trim_slash container ^ "/" ^ base in
        let v = Ty.set "basename" (Untyped_tree.String base) v in
        Ok (set_paths v (Some base, Some seen), base :: used)
  | _ -> Ok (v, used)

let stage_value (module R : Io.WRITE) ~container ~outdir used v =
  Ty.fold_map (stage_leaf (module R) ~container ~outdir) used v

let stage (module R : Io.WRITE) ~container ~outdir inputs =
  let rec go used acc = function
    | [] -> Ok (List.rev acc)
    | (k, v) :: rest ->
        let* v, used = stage_value (module R) ~container ~outdir used v in
        go used ((k, v) :: acc) rest
  in
  go [] [] inputs

let relocate ~container ~host path =
  let dotdot = List.mem ".." (String.split_on_char '/' path) in
  if container = host || Filename.is_relative path || dotdot then path
  else
    match Location.relative container (Location.normalize path) with
    | None -> path
    | Some "" -> Location.trim_slash host
    | Some rest -> Location.trim_slash host ^ "/" ^ rest

let resolve_paths ~container host v =
  let abs p =
    let p = relocate ~container ~host p in
    if Filename.is_relative p then Filename.concat host p else p
  in
  let endpoints preferred location =
    let path = Option.map abs preferred in
    let location =
      match Option.map (fun l -> abs (Location.to_path l)) location with
      | Some _ as kept -> kept
      | None -> path
    in
    (path, location)
  in
  Ty.map
    (fun v -> set_paths v (endpoints (Ty.path v) (Ty.get_string "location" v)))
    v

let kind_name = function `File -> "File" | `Directory -> "Directory"

let got_name expected = function
  | `File -> "File"
  | `Directory -> "Directory"
  | `Not_found | `Symlink | `Other -> (
      match expected with
      | `File -> "not a file"
      | `Directory -> "not a directory")

let node_matches path expected got =
  match (expected, got) with
  | `File, `File | `Directory, `Directory -> Ok ()
  | expected, got ->
      Error
        (Error.Type
           {
             param = path;
             expected = kind_name expected;
             got = got_name expected got;
           })

let confine (module R : Io.READ) roots v =
  let confine_path raw =
    if raw = "" then Error.runtime "File or Directory output missing path"
    else
      let* () = R.confined roots raw in
      match R.realpath raw with Ok p -> Ok p | Error _ -> Ok raw
  in
  let one v =
    match Ty.class_of v with
    | None -> Ok v
    | Some cls ->
        let kind = if cls = "File" then `File else `Directory in
        let* path = confine_path (Option.value (Ty.path v) ~default:"") in
        let+ () = node_matches path kind (R.stat path) in
        Ty.fill_file_paths (set_paths v (Some path, Some path))
  in
  Ty.map_result one v

(* [v] with [key] set to what [compute] gives, unless [v] already has it. *)
let or_fill key compute v =
  if Ty.get key v <> None then Ok v
  else
    let+ x = compute () in
    Ty.set key x v

let checksum (module R : Io.READ) v =
  Ty.map_result
    (fun v ->
      match (Ty.class_of v, Ty.path v) with
      | Some "File", Some path ->
          let sha1 () =
            let+ hex = R.sha1 path in
            Untyped_tree.String ("sha1$" ^ hex)
          in
          let size () =
            let+ n = R.file_size path in
            Untyped_tree.Int n
          in
          let* v = or_fill "checksum" sha1 v in
          or_fill "size" size v
      | _ -> Ok v)
    v
