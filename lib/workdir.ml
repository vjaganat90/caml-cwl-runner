(** File and Directory values against a work directory. Stages inputs, relocates
    container paths, confines outputs, and fills checksums. Does not glob,
    spawn, or build argv. *)

open Error.Syntax

let safe_leaf ~what s =
  if s = "" || s = "." || s = ".." then
    Error.runtime
      (Printf.sprintf "%s must be a single path segment, got %S" what s)
  else if
    String.contains s '/' || String.contains s '\\' || String.contains s '\000'
  then
    Error.runtime
      (Printf.sprintf "%s must be a single path segment, got %S" what s)
  else Ok s

let strip_file_uri loc =
  if String.starts_with ~prefix:"file://" loc then
    let rest = String.sub loc 7 (String.length loc - 7) in
    if String.starts_with ~prefix:"//" rest then
      match String.index_from_opt rest 2 '/' with
      | Some i -> String.sub rest i (String.length rest - i)
      | None -> rest
    else rest
  else loc

let ensure_local_path s =
  match String.index_opt s ':' with
  | Some i
    when i > 0 && String.length s > i + 2 && s.[i + 1] = '/' && s.[i + 2] = '/'
    ->
      let scheme = String.lowercase_ascii (String.sub s 0 i) in
      if scheme = "file" then Ok (strip_file_uri s)
      else
        Error.runtime (Printf.sprintf "unsupported location scheme %S" scheme)
  | _ -> Ok s

let resolve ~job_dir loc =
  let loc = strip_file_uri loc in
  if Filename.is_relative loc then Filename.concat job_dir loc else loc

let stage_leaf (module R : Runtime.WRITE) ~container ~outdir ~job_dir used =
  function
  | Ty.Vfile f ->
      let src = Option.map (resolve ~job_dir) (Ty.file_loc f) in
      let* src =
        match src with
        | None -> Error.runtime "File input missing location"
        | Some s -> ensure_local_path s
      in
      let raw_base = Option.value f.basename ~default:(Filename.basename src) in
      let* base = safe_leaf ~what:"File basename" raw_base in
      if List.mem base used then
        Error.runtime (Printf.sprintf "staging collision on basename '%s'" base)
      else
        let dst = Filename.concat outdir base in
        let* () = R.copy_file src dst in
        let seen =
          let n = String.length container in
          let container =
            if n > 1 && container.[n - 1] = '/' then
              String.sub container 0 (n - 1)
            else container
          in
          container ^ "/" ^ base
        in
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
      let src = Option.map (resolve ~job_dir) (Ty.dir_loc d) in
      let* src =
        match src with
        | None -> Error.runtime "Directory input missing location"
        | Some s -> ensure_local_path s
      in
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

let trim_slash s =
  let n = String.length s in
  if n > 1 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

let normalize_abs path =
  let segs = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
  let rec go acc = function
    | [] -> Some (List.rev acc)
    | "." :: rest -> go acc rest
    | ".." :: _ -> None
    | seg :: rest -> go (seg :: acc) rest
  in
  match go [] segs with
  | None -> None
  | Some [] -> Some "/"
  | Some segs -> Some ("/" ^ String.concat "/" segs)

let relocate ~container ~host path =
  if container = host || not (String.starts_with ~prefix:"/" path) then path
  else
    match normalize_abs path with
    | None -> path
    | Some normalized ->
        let container = trim_slash container in
        let host = trim_slash host in
        if normalized = container then host
        else
          let prefix = container ^ "/" in
          if String.starts_with ~prefix normalized then
            host
            ^ String.sub normalized (String.length container)
                (String.length normalized - String.length container)
          else path

let resolve_paths ~container host v =
  let abs p =
    let p = strip_file_uri p in
    let p = relocate ~container ~host p in
    if Filename.is_relative p then Filename.concat host p else p
  in
  Ty.map
    (function
      | Ty.Vfile f ->
          let path = Option.map abs (Ty.file_path f) in
          let location = Option.map abs f.location in
          let location =
            match location with Some _ as l -> l | None -> path
          in
          Ty.Vfile { f with path; location }
      | Ty.Vdir d ->
          let path = Option.map abs (Ty.dir_path d) in
          let location = Option.map abs d.location in
          let location =
            match location with Some _ as l -> l | None -> path
          in
          Ty.Vdir { path; location }
      | v -> v)
    v

let node_matches path expected got =
  match (expected, got) with
  | `File, `File -> Ok ()
  | `Directory, `Directory -> Ok ()
  | `File, `Directory ->
      Error (Error.Type { param = path; expected = "File"; got = "Directory" })
  | `Directory, `File ->
      Error (Error.Type { param = path; expected = "Directory"; got = "File" })
  | `File, _ ->
      Error (Error.Type { param = path; expected = "File"; got = "not a file" })
  | `Directory, _ ->
      Error
        (Error.Type
           { param = path; expected = "Directory"; got = "not a directory" })

let confine (module R : Runtime.READ) roots v =
  let confine_path raw =
    if raw = "" then Error.runtime "File or Directory output missing path"
    else
      let* () = R.confined roots raw in
      match R.realpath raw with Ok p -> Ok p | Error _ -> Ok raw
  in
  Ty.map_result
    (function
      | Ty.Vfile f ->
          let* path =
            confine_path (Option.value (Ty.file_path f) ~default:"")
          in
          let* () = node_matches path `File (R.stat path) in
          Ok
            (Ty.fill_file_paths
               (Ty.Vfile { f with path = Some path; location = Some path }))
      | Ty.Vdir d ->
          let* path = confine_path (Option.value (Ty.dir_path d) ~default:"") in
          let+ () = node_matches path `Directory (R.stat path) in
          Ty.Vdir { path = Some path; location = Some path }
      | v -> Ok v)
    v

let checksum (module R : Runtime.READ) v =
  Ty.map_result
    (function
      | Ty.Vfile f -> (
          match Ty.file_path f with
          | None -> Ok (Ty.Vfile f)
          | Some path ->
              let* checksum =
                match f.checksum with
                | Some _ as c -> Ok c
                | None ->
                    let+ hex = R.sha1 path in
                    Some ("sha1$" ^ hex)
              in
              let* size =
                match f.size with
                | Some _ as n -> Ok n
                | None ->
                    let+ n = R.file_size path in
                    Some n
              in
              Ok (Ty.Vfile { f with checksum; size }))
      | v -> Ok v)
    v
