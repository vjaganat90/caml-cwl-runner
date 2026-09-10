module type RUNTIME = sig
  include Glob.FS

  val mkdir_p : string -> (unit, Error.t) result
  val abspath : string -> (string, Error.t) result
  val mkdtemp : prefix:string -> (string, Error.t) result
  val copy_file : src:string -> dst:string -> (unit, Error.t) result
  val read_file : string -> (string, Error.t) result
  val write_file : string -> string -> (unit, Error.t) result
  val file_size : string -> (int64, Error.t) result

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

let local env =
  let fs = Eio.Stdenv.fs env in
  let cwd_path = Eio.Stdenv.cwd env in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let ( / ) = Eio.Path.( / ) in
  let p s = if Filename.is_relative s then cwd_path / s else fs / s in
  (module struct
    let exists s =
      match Eio.Path.kind ~follow:true (p s) with
      | `Not_found -> false
      | _ -> true

    let is_dir s = Eio.Path.is_directory (p s)
    let read_dir s = wrap (fun () -> Eio.Path.read_dir (p s))

    let mkdir_p s =
      wrap (fun () -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 (p s))

    let abspath s =
      wrap (fun () ->
          match Eio.Path.native (p s) with
          | Some n -> n
          | None ->
              if Filename.is_relative s then Filename.concat (Sys.getcwd ()) s
              else s)

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
