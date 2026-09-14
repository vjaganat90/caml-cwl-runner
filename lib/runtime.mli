(** Stage files and spawn processes. The local body is Eio; tests inject fakes.
*)

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

val local : Eio_unix.Stdenv.base -> (module RUNTIME)
