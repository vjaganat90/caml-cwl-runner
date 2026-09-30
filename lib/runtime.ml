(** Stage files and spawn processes. The local body is Eio; tests pack a fake.
    Child cwd is the CWL outdir. Does not parse CWL or build argv. *)

open Error.Syntax

type node = [ `Not_found | `File | `Directory | `Symlink | `Other ]

type stdio = {
  stdin_file : string option;
  stdout_file : string option;
  stderr_file : string option;
}

type tool_env = { home : string; tmpdir : string; path : string option }

module type RUNTIME = sig
  include Glob.FS

  val mkdir_p : string -> (unit, Error.t) result
  val abspath : string -> (string, Error.t) result
  val mkdtemp : string -> (string, Error.t) result
  val copy_file : string -> string -> (unit, Error.t) result
  val read_file : string -> (string, Error.t) result
  val write_file : string -> string -> (unit, Error.t) result
  val file_size : string -> (int64, Error.t) result
  val sha1 : string -> (string, Error.t) result
  val remove_tree : string -> (unit, Error.t) result
  val lstat : string -> node
  val stat : string -> node
  val confined : string list -> string -> (unit, Error.t) result

  val spawn :
    tool_env -> string -> stdio -> string list -> (int, Error.t) result
end

let rt_err message = Error (Error.Runtime { message })
let wrap f = try Ok (f ()) with exn -> rt_err (Printexc.to_string exn)

let confined_using realpath roots path =
  if roots = [] then rt_err "confined: no roots"
  else
    match realpath path with
    | Error _ as e -> e
    | Ok resolved ->
        let rec go = function
          | [] ->
              rt_err (Printf.sprintf "path %S is not under a legal root" path)
          | root :: rest -> (
              match realpath root with
              | Ok r when Glob.under r resolved -> Ok ()
              | Ok _ | Error _ -> go rest)
        in
        go roots

let node_of = function
  | `Not_found -> `Not_found
  | `Regular_file -> `File
  | `Directory -> `Directory
  | `Symbolic_link -> `Symlink
  | `Unknown | `Fifo | `Character_special | `Block_device | `Socket -> `Other

type console = Eio.Flow.sink_ty Eio.Resource.t

let default_console env = (Eio.Stdenv.stderr env :> console)

let tool_env ~outdir ~tmpdir =
  { home = outdir; tmpdir; path = Sys.getenv_opt "PATH" }

let env_list e =
  [ "HOME=" ^ e.home; "TMPDIR=" ^ e.tmpdir ]
  @ match e.path with None -> [] | Some p -> [ "PATH=" ^ p ]

(* What to start: a launcher turns the tool's cwd, argv, and tool_env into
   the process that actually runs, with that process's own environment. *)
type launch = { cwd : string; argv : string list; env : string list }

let path_of eio s =
  let ( / ) = Eio.Path.( / ) in
  if Filename.is_relative s then Eio.Stdenv.cwd eio / s
  else Eio.Stdenv.fs eio / s

let run_process eio (console : console) { cwd; argv; env }
    ({ stdin_file; stdout_file; stderr_file } : stdio) =
  wrap (fun () ->
      Eio.Switch.run @@ fun sw ->
      let p = path_of eio in
      let open_in = function
        | None -> (Eio.Path.open_in ~sw (p "/dev/null") :> _ Eio.Flow.source)
        | Some f -> (Eio.Path.open_in ~sw (p f) :> _ Eio.Flow.source)
      in
      let open_out = function
        | None -> console
        | Some f ->
            (Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644) (p f)
              :> console)
      in
      let proc =
        Eio.Process.spawn ~sw
          (Eio.Stdenv.process_mgr eio)
          ~cwd:(p cwd) ~env:(Array.of_list env) ~stdin:(open_in stdin_file)
          ~stdout:(open_out stdout_file) ~stderr:(open_out stderr_file) argv
      in
      match Eio.Process.await proc with
      | `Exited n -> n
      | `Signaled s -> failwith (Printf.sprintf "process killed by signal %d" s))

let filesystem eio (console : console) launch =
  let p = path_of eio in
  let native s =
    match Eio.Path.native (p s) with
    | Some n -> n
    | None -> failwith (Printf.sprintf "not a native path: %s" s)
  in
  (module struct
    let exists s =
      match Eio.Path.kind ~follow:true (p s) with
      | `Not_found -> false
      | _ -> true

    let is_dir s = Eio.Path.is_directory (p s)
    let read_dir s = wrap (fun () -> Eio.Path.read_dir (p s))

    let mkdir_p s =
      wrap (fun () -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 (p s))

    let abspath s = wrap (fun () -> native s)
    let mkdtemp prefix = wrap (fun () -> Filename.temp_dir prefix "")
    let read_file s = wrap (fun () -> Eio.Path.load (p s))

    let write_file s data =
      wrap (fun () -> Eio.Path.save ~create:(`Or_truncate 0o644) (p s) data)

    let file_size s =
      wrap (fun () ->
          let st = Eio.Path.stat ~follow:true (p s) in
          Optint.Int63.to_int64 st.size)

    let sha1 s =
      wrap (fun () ->
          In_channel.with_open_bin (native s) @@ fun ic ->
          let buf = Bytes.create 65536 in
          let rec go ctx =
            match In_channel.input ic buf 0 (Bytes.length buf) with
            | 0 -> ctx
            | len -> go (Digestif.SHA1.feed_bytes ctx ~off:0 ~len buf)
          in
          Digestif.SHA1.(to_hex (get (go empty))))

    let remove_tree s = wrap (fun () -> Eio.Path.rmtree ~missing_ok:true (p s))

    let lstat s =
      try node_of (Eio.Path.kind ~follow:false (p s)) with _ -> `Other

    let stat s =
      try node_of (Eio.Path.kind ~follow:true (p s)) with _ -> `Other

    let copy_file src dst =
      match stat src with
      | `File ->
          wrap (fun () ->
              Eio.Path.with_open_in (p src) @@ fun inn ->
              Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (p dst)
              @@ fun out -> Eio.Flow.copy inn out)
      | `Not_found -> rt_err (Printf.sprintf "copy source not found: %s" src)
      | _ -> rt_err (Printf.sprintf "copy source is not a regular file: %s" src)

    let realpath s = wrap (fun () -> Unix.realpath (native s))
    let confined roots path = confined_using realpath roots path

    let reject_stdio_symlink label = function
      | None -> Ok ()
      | Some f -> (
          match lstat f with
          | `Symlink ->
              rt_err (Printf.sprintf "%s destination is a symlink" label)
          | _ -> Ok ())

    let spawn env cwd (stdio : stdio) argv =
      if argv = [] then rt_err "empty argv"
      else
        let* () = reject_stdio_symlink "stdin" stdio.stdin_file in
        let* () = reject_stdio_symlink "stdout" stdio.stdout_file in
        let* () = reject_stdio_symlink "stderr" stdio.stderr_file in
        let* launched = launch env cwd argv in
        run_process eio console launched stdio
  end : RUNTIME)

let local ?console env =
  let console = Option.value console ~default:(default_console env) in
  filesystem env console (fun tool cwd argv ->
      Ok { cwd; argv; env = env_list tool })

let docker_executable () =
  let candidates =
    [
      "/Applications/Docker.app/Contents/Resources/bin/docker";
      "/opt/homebrew/bin/docker";
      "/usr/local/bin/docker";
    ]
  in
  if Sys.command "command -v docker >/dev/null 2>&1" = 0 then "docker"
  else
    match List.find_opt Sys.file_exists candidates with
    | Some path -> path
    | None -> "docker"

type docker_spec = {
  bin : string;
  user : string;
  cwd : string;
  workdir : string;
  image : string;
  mounts : (string * string) list;
  container_env : string list;
}

let mount_target s =
  match Schema.Container_outdir.of_string s with
  | Ok path -> Ok (path :> string)
  | Error message -> rt_err ("docker mount path " ^ message)

let docker_mount ~host ~workdir =
  let* source =
    try Ok (Unix.realpath host) with exn -> rt_err (Printexc.to_string exn)
  in
  let* source = mount_target source in
  let+ workdir = mount_target workdir in
  (source, workdir)

let docker_run_argv spec argv =
  [
    spec.bin;
    "run";
    "--rm";
    "--user";
    spec.user;
    "-v";
    spec.cwd ^ ":" ^ spec.workdir;
  ]
  @ List.concat_map (fun (src, dst) -> [ "-v"; src ^ ":" ^ dst ]) spec.mounts
  @ List.concat_map (fun e -> [ "--env"; e ]) spec.container_env
  @ [ "-w"; spec.workdir; spec.image ]
  @ argv

(* The docker client runs with the invoking environment: its config,
   contexts, and credential helpers live under the user's HOME. When [bin] is
   an absolute path, its directory goes first on PATH so helpers installed
   beside it are found. *)
let client_env bin =
  let host = Array.to_list (Unix.environment ()) in
  if Filename.is_relative bin then host
  else
    let dir = Filename.dirname bin in
    let path =
      match Sys.getenv_opt "PATH" with Some p -> dir ^ ":" ^ p | None -> dir
    in
    ("PATH=" ^ path)
    :: List.filter (fun e -> not (String.starts_with ~prefix:"PATH=" e)) host

let run_docker eio console argv =
  let out = Filename.temp_file "ccr-docker-" ".txt" in
  let bin = match argv with b :: _ -> b | [] -> "" in
  let* code =
    run_process eio console
      { cwd = "/"; argv; env = client_env bin }
      { stdin_file = None; stdout_file = Some out; stderr_file = None }
  in
  let text = In_channel.with_open_bin out In_channel.input_all in
  Sys.remove out;
  if code <> 0 then
    rt_err
      (Printf.sprintf "docker command failed (%d): %s" code (String.trim text))
  else Ok text

let is_http s =
  String.starts_with ~prefix:"http://" s
  || String.starts_with ~prefix:"https://" s

let fetch env console source =
  if is_http source then
    let dest = Filename.temp_file "ccr-docker-" ".img" in
    let+ _ = run_docker env console [ "curl"; "-fsSL"; "-o"; dest; source ] in
    dest
  else if Sys.file_exists source then Ok source
  else rt_err (Printf.sprintf "docker image source not found: %s" source)

let gunzip_if_needed path =
  if String.ends_with ~suffix:".gz" path || String.ends_with ~suffix:".tgz" path
  then
    let dest = Filename.temp_file "ccr-docker-" ".tar" in
    let code =
      Sys.command
        (Printf.sprintf "gzip -dc %s > %s" (Filename.quote path)
           (Filename.quote dest))
    in
    if code <> 0 then rt_err (Printf.sprintf "gunzip failed (%d)" code)
    else Ok dest
  else Ok path

let loaded_name text =
  let rec find = function
    | [] -> None
    | line :: rest -> (
        let line = String.trim line in
        let take prefix =
          let n = String.length prefix in
          if String.starts_with ~prefix line then
            Some (String.trim (String.sub line n (String.length line - n)))
          else None
        in
        match take "Loaded image: " with
        | Some _ as n -> n
        | None -> (
            match take "Loaded image ID: " with
            | Some _ as n -> n
            | None -> find rest))
  in
  find (String.split_on_char '\n' text)

let tag_of name contents =
  match name with
  | Some tag -> tag
  | None -> Printf.sprintf "ccr:%x" (Hashtbl.hash contents)

let prepare_image env console image =
  let bin = docker_executable () in
  match image with
  | Schema.Pull name ->
      let+ _ = run_docker env console [ bin; "pull"; name ] in
      name
  | Schema.Image_id id -> Ok id
  | Schema.Load { source; name } -> (
      let* path = fetch env console source in
      let* text = run_docker env console [ bin; "load"; "-i"; path ] in
      match name with
      | Some tag -> Ok tag
      | None -> (
          match loaded_name text with
          | Some tag -> Ok tag
          | None ->
              rt_err
                (Printf.sprintf "docker load did not name an image: %s"
                   (String.trim text))))
  | Schema.Import { source; name } ->
      let* path = fetch env console source in
      let* tar = gunzip_if_needed path in
      let tag = tag_of name source in
      let+ _ = run_docker env console [ bin; "import"; tar; tag ] in
      tag
  | Schema.Dockerfile { contents; tag } ->
      let dir = Filename.temp_dir "ccr-docker-" "" in
      let dockerfile = Filename.concat dir "Dockerfile" in
      let oc = Out_channel.open_text dockerfile in
      Out_channel.output_string oc contents;
      Out_channel.close oc;
      let tag = tag_of tag contents in
      let+ _ = run_docker env console [ bin; "build"; "-t"; tag; dir ] in
      tag

let docker ?console env (req : Schema.docker) =
  let console = Option.value console ~default:(default_console env) in
  filesystem env console (fun (tool : tool_env) cwd argv ->
      let* tag = prepare_image env console req.image in
      let user = Printf.sprintf "%d:%d" (Unix.getuid ()) (Unix.getgid ()) in
      let bin = docker_executable () in
      let workdir =
        match req.output_directory with
        | None -> cwd
        | Some path -> (path :> string)
      in
      let* source, workdir = docker_mount ~host:cwd ~workdir in
      let* tmp = docker_mount ~host:tool.tmpdir ~workdir:tool.tmpdir in
      let container_env = [ "HOME=" ^ tool.home; "TMPDIR=" ^ tool.tmpdir ] in
      let spec =
        {
          bin;
          user;
          cwd = source;
          workdir;
          image = tag;
          mounts = [ tmp ];
          container_env;
        }
      in
      Ok
        ({ cwd = "/"; argv = docker_run_argv spec argv; env = client_env bin }
          : launch))
