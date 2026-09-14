type node = [ `Not_found | `File | `Directory | `Symlink | `Other ]

module type RUNTIME = sig
  include Glob.FS

  val mkdir_p : string -> (unit, Error.t) result
  val abspath : string -> (string, Error.t) result
  val mkdtemp : prefix:string -> (string, Error.t) result
  val copy_file : src:string -> dst:string -> (unit, Error.t) result
  val read_file : string -> (string, Error.t) result
  val write_file : string -> string -> (unit, Error.t) result
  val file_size : string -> (int64, Error.t) result
  val lstat : string -> node
  val stat : string -> node
  val realpath : string -> (string, Error.t) result
  val confined : roots:string list -> path:string -> (unit, Error.t) result

  val spawn :
    cwd:string ->
    stdin_file:string option ->
    stdout_file:string option ->
    stderr_file:string option ->
    argv:string list ->
    (int, Error.t) result
end

let rt_err message = Error (Error.Runtime { message })
let wrap f = try Ok (f ()) with exn -> rt_err (Printexc.to_string exn)

let trim_slash s =
  let n = String.length s in
  if n > 1 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

let under ~root path =
  let root = trim_slash root in
  path = root || String.starts_with ~prefix:(root ^ "/") path

let confined_using realpath ~roots path =
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
              | Ok r when under ~root:r resolved -> Ok ()
              | Ok _ | Error _ -> go rest)
        in
        go roots

let node_of = function
  | `Not_found -> `Not_found
  | `Regular_file -> `File
  | `Directory -> `Directory
  | `Symbolic_link -> `Symlink
  | `Unknown | `Fifo | `Character_special | `Block_device | `Socket -> `Other

let local env =
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
    let mkdtemp ~prefix = wrap (fun () -> Filename.temp_dir prefix "")

    let copy_file ~src ~dst =
      wrap (fun () ->
          Eio.Path.with_open_in (p src) @@ fun inn ->
          Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (p dst)
          @@ fun out -> Eio.Flow.copy inn out)

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

    let realpath s = wrap (fun () -> Unix.realpath (native s))
    let confined ~roots ~path = confined_using realpath ~roots path

    let spawn ~cwd ~stdin_file ~stdout_file ~stderr_file ~argv =
      if argv = [] then rt_err "empty argv"
      else
        wrap (fun () ->
            Eio.Switch.run @@ fun sw ->
            let open_in = function
              | None ->
                  (Eio.Path.open_in ~sw (p "/dev/null") :> _ Eio.Flow.source)
              | Some f -> (Eio.Path.open_in ~sw (p f) :> _ Eio.Flow.source)
            in
            let open_out = function
              | None ->
                  (Eio.Path.open_out ~sw ~create:`Never (p "/dev/null")
                    :> _ Eio.Flow.sink)
              | Some f ->
                  (Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644) (p f)
                    :> _ Eio.Flow.sink)
            in
            let proc =
              Eio.Process.spawn ~sw proc_mgr ~cwd:(p cwd)
                ~stdin:(open_in stdin_file) ~stdout:(open_out stdout_file)
                ~stderr:(open_out stderr_file) argv
            in
            match Eio.Process.await proc with
            | `Exited n -> n
            | `Signaled s ->
                failwith (Printf.sprintf "process killed by signal %d" s))
  end : RUNTIME)
