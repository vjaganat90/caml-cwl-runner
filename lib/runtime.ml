(** Stage files and spawn processes. The local body is Eio; tests pack a fake.
    Child cwd is the CWL outdir. Does not parse CWL or build argv. *)

type node = [ `Not_found | `File | `Directory | `Symlink | `Other ]

type stdio = {
  stdin_file : string option;
  stdout_file : string option;
  stderr_file : string option;
}

module type RUNTIME = sig
  include Glob.FS

  val mkdir_p : string -> (unit, Error.t) result
  val abspath : string -> (string, Error.t) result
  val mkdtemp : string -> (string, Error.t) result
  val copy_file : string -> string -> (unit, Error.t) result
  val read_file : string -> (string, Error.t) result
  val write_file : string -> string -> (unit, Error.t) result
  val file_size : string -> (int64, Error.t) result
  val lstat : string -> node
  val stat : string -> node
  val realpath : string -> (string, Error.t) result
  val confined : string list -> string -> (unit, Error.t) result

  val spawn :
    env:string list -> string -> stdio -> string list -> (int, Error.t) result
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

type launch = { cwd : string; argv : string list }

let filesystem env ~launch =
  let fs = Eio.Stdenv.fs env in
  let cwd_path = Eio.Stdenv.cwd env in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let ( / ) = Eio.Path.( / ) in
  let p s = if Filename.is_relative s then cwd_path / s else fs / s in
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

    let spawn ~env cwd ({ stdin_file; stdout_file; stderr_file } : stdio) argv =
      if argv = [] then rt_err "empty argv"
      else
        match
          let ( let* ) = Result.bind in
          let* () = reject_stdio_symlink "stdin" stdin_file in
          let* () = reject_stdio_symlink "stdout" stdout_file in
          let* () = reject_stdio_symlink "stderr" stderr_file in
          Ok ()
        with
        | Error _ as e -> e
        | Ok () -> (
            match launch cwd argv with
            | Error _ as e -> e
            | Ok launched ->
                wrap (fun () ->
                    Eio.Switch.run @@ fun sw ->
                    let open_in = function
                      | None ->
                          (Eio.Path.open_in ~sw (p "/dev/null")
                            :> _ Eio.Flow.source)
                      | Some f ->
                          (Eio.Path.open_in ~sw (p f) :> _ Eio.Flow.source)
                    in
                    let open_out = function
                      | None ->
                          (Eio.Path.open_out ~sw ~create:`Never (p "/dev/null")
                            :> _ Eio.Flow.sink)
                      | Some f ->
                          (Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644)
                             (p f)
                            :> _ Eio.Flow.sink)
                    in
                    let env =
                      match launched.argv with
                      | exe :: _
                        when (not (Filename.is_relative exe))
                             && Filename.basename exe = "docker" ->
                          let dir = Filename.dirname exe in
                          let path =
                            match Sys.getenv_opt "PATH" with
                            | Some p -> dir ^ ":" ^ p
                            | None -> dir
                          in
                          Unix.environment () |> Array.to_list
                          |> List.filter (fun e ->
                              not (String.starts_with ~prefix:"PATH=" e))
                          |> List.cons ("PATH=" ^ path)
                          |> Array.of_list
                      | _ -> Array.of_list env
                    in
                    let proc =
                      Eio.Process.spawn ~sw proc_mgr ~cwd:(p launched.cwd) ~env
                        ~stdin:(open_in stdin_file)
                        ~stdout:(open_out stdout_file)
                        ~stderr:(open_out stderr_file) launched.argv
                    in
                    match Eio.Process.await proc with
                    | `Exited n -> n
                    | `Signaled s ->
                        failwith
                          (Printf.sprintf "process killed by signal %d" s)))
  end : RUNTIME)

let tool_env ~outdir ~tmpdir =
  let env = [ "HOME=" ^ outdir; "TMPDIR=" ^ tmpdir ] in
  match Sys.getenv_opt "PATH" with
  | None -> env
  | Some path -> env @ [ "PATH=" ^ path ]

let local env = filesystem env ~launch:(fun cwd argv -> Ok { cwd; argv })

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
}

let mount_target s =
  match Schema.Container_outdir.of_string s with
  | Ok path -> Ok (Schema.Container_outdir.to_string path)
  | Error message -> rt_err ("docker mount path " ^ message)

let docker_mount ~host ~workdir =
  let ( let* ) = Error.( let* ) in
  let* source =
    try Ok (Unix.realpath host) with exn -> rt_err (Printexc.to_string exn)
  in
  let* source = mount_target source in
  let* workdir = mount_target workdir in
  Ok (source, workdir)

let docker_run_argv spec argv =
  [
    spec.bin;
    "run";
    "--rm";
    "--user";
    spec.user;
    "-v";
    spec.cwd ^ ":" ^ spec.workdir;
    "-w";
    spec.workdir;
    spec.image;
  ]
  @ argv

let run_docker env argv =
  let ( let* ) = Error.( let* ) in
  let (module Host : RUNTIME) = local env in
  let out = Filename.temp_file "ccr-docker-" ".txt" in
  let* code =
    Host.spawn
      ~env:(Unix.environment () |> Array.to_list)
      "/"
      { stdin_file = None; stdout_file = Some out; stderr_file = None }
      argv
  in
  let* text = Host.read_file out in
  Sys.remove out;
  if code <> 0 then
    rt_err
      (Printf.sprintf "docker command failed (%d): %s" code (String.trim text))
  else Ok text

let is_http s =
  String.starts_with ~prefix:"http://" s
  || String.starts_with ~prefix:"https://" s

let fetch env source =
  let ( let* ) = Error.( let* ) in
  if is_http source then
    let dest = Filename.temp_file "ccr-docker-" ".img" in
    let* _ = run_docker env [ "curl"; "-fsSL"; "-o"; dest; source ] in
    Ok dest
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

let prepare_image env image =
  let ( let* ) = Error.( let* ) in
  let bin = docker_executable () in
  match image with
  | Schema.Pull name ->
      let* _ = run_docker env [ bin; "pull"; name ] in
      Ok name
  | Schema.Image_id id -> Ok id
  | Schema.Load { source; name } -> (
      let* path = fetch env source in
      let* text = run_docker env [ bin; "load"; "-i"; path ] in
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
      let* path = fetch env source in
      let* tar = gunzip_if_needed path in
      let tag = tag_of name source in
      let* _ = run_docker env [ bin; "import"; tar; tag ] in
      Ok tag
  | Schema.Dockerfile { contents; tag } ->
      let dir = Filename.temp_dir "ccr-docker-" "" in
      let dockerfile = Filename.concat dir "Dockerfile" in
      let oc = Out_channel.open_text dockerfile in
      Out_channel.output_string oc contents;
      Out_channel.close oc;
      let tag = tag_of tag contents in
      let* _ = run_docker env [ bin; "build"; "-t"; tag; dir ] in
      Ok tag

let docker env (req : Schema.docker) =
  let ( let* ) = Error.( let* ) in
  filesystem env ~launch:(fun cwd argv ->
      let* tag = prepare_image env req.image in
      let user = Printf.sprintf "%d:%d" (Unix.getuid ()) (Unix.getgid ()) in
      let bin = docker_executable () in
      let workdir =
        match req.output_directory with
        | None -> cwd
        | Some path -> Schema.Container_outdir.to_string path
      in
      let* source, workdir = docker_mount ~host:cwd ~workdir in
      Ok
        {
          cwd = "/";
          argv =
            docker_run_argv
              { bin; user; cwd = source; workdir; image = tag }
              argv;
        })
