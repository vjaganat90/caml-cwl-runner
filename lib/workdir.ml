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
let source ~job_dir ~what location path =
  let+ src =
    match (location, path) with
    | Some loc, _ -> local_path loc
    | None, Some path -> Ok path
    | None, None ->
        Error.runtime (Printf.sprintf "%s input missing location" what)
  in
  if Filename.is_relative src then Filename.concat job_dir src else src

let stage_leaf (module R : Runtime.WRITE) ~container ~outdir ~job_dir used =
  function
  | Ty.Vfile f ->
      let* src = source ~job_dir ~what:"File" f.location f.path in
      let raw_base = Option.value f.basename ~default:(Filename.basename src) in
      let* base = safe_leaf ~what:"File basename" raw_base in
      if List.mem base used then
        Error.runtime (Printf.sprintf "staging collision on basename '%s'" base)
      else
        let dst = Filename.concat outdir base in
        let* () = R.copy_file src dst in
        let seen = Location.trim_slash container ^ "/" ^ base in
        Ok
          ( Ty.Vfile
              {
                f with
                path = Some base;
                basename = Some base;
                location = Some seen;
              },
            base :: used )
  | Ty.Vdir d ->
      let* src = source ~job_dir ~what:"Directory" d.location d.path in
      Ok (Ty.Vdir { location = Some src; path = Some src }, used)
  | v -> Ok (v, used)

let stage_value (module R : Runtime.WRITE) ~container ~outdir ~job_dir used v =
  Ty.fold_map (stage_leaf (module R) ~container ~outdir ~job_dir) used v

let stage (module R : Runtime.WRITE) ~container ~outdir ~job_dir inputs =
  let rec go used acc = function
    | [] -> Ok (List.rev acc)
    | (k, v) :: rest ->
        let* v, used =
          stage_value (module R) ~container ~outdir ~job_dir used v
        in
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

let map_paths f = function
  | Ty.Vfile file ->
      let path, location = f (Ty.file_path file) file.location in
      Ty.Vfile { file with path; location }
  | Ty.Vdir dir ->
      let path, location = f (Ty.dir_path dir) dir.location in
      Ty.Vdir { path; location }
  | v -> v

let map_paths_result f = function
  | Ty.Vfile file ->
      let+ path, location = f `File (Ty.file_path file) in
      Ty.fill_file_paths (Ty.Vfile { file with path; location })
  | Ty.Vdir dir ->
      let+ path, location = f `Directory (Ty.dir_path dir) in
      Ty.Vdir { path; location }
  | v -> Ok v

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
  Ty.map (map_paths endpoints) v

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

let confine (module R : Runtime.READ) roots v =
  let confine_path raw =
    if raw = "" then Error.runtime "File or Directory output missing path"
    else
      let* () = R.confined roots raw in
      match R.realpath raw with Ok p -> Ok p | Error _ -> Ok raw
  in
  let one kind preferred =
    let* path = confine_path (Option.value preferred ~default:"") in
    let+ () = node_matches path kind (R.stat path) in
    (Some path, Some path)
  in
  Ty.map_result (map_paths_result one) v

let or_fill present compute =
  match present with
  | Some _ as kept -> Ok kept
  | None ->
      let+ value = compute () in
      Some value

let checksum (module R : Runtime.READ) v =
  Ty.map_result
    (function
      | Ty.Vfile f -> (
          match Ty.file_path f with
          | None -> Ok (Ty.Vfile f)
          | Some path ->
              let* checksum =
                or_fill f.checksum (fun () ->
                    let+ hex = R.sha1 path in
                    "sha1$" ^ hex)
              in
              let* size = or_fill f.size (fun () -> R.file_size path) in
              Ok (Ty.Vfile { f with checksum; size }))
      | v -> Ok v)
    v
