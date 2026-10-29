(** The world outside the gate: processes, files, the git repository, and the Markdown report. Every other module reads
    files and runs commands through here. *)

exception Fail of string
(** A problem that stops the gate, such as a missing ref or a malformed mutant. [Gates] prints it and exits 1. *)

let fail fmt = Printf.ksprintf (fun s -> raise (Fail s)) fmt
let slurp path = In_channel.with_open_bin path In_channel.input_all
let split c s = List.filter (( <> ) "") (String.split_on_char c s)
let lines = split '\n'
let nonblank s = List.filter (fun l -> String.trim l <> "") (String.split_on_char '\n' s)
let words s = List.filter (( <> ) "") (Re.split (Re.Perl.compile_pat {|\s+|}) s)
let find s text = Re.all (Re.compile (Re.str s)) text
let contains s text = find s text <> []
let newlines s = String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 s
let under prefixes p = List.exists (fun prefix -> String.starts_with ~prefix p) prefixes

(** [[ msg ]] when [cond] holds, else none. *)
let when_ cond msg = if cond then [ msg ] else []

(** [items >? msg] is one problem, [msg] of the items joined, when there are any. *)
let ( >? ) items msg = when_ (items <> []) (msg (String.concat ", " items))

(** The environment with each [(name, value)] of [vars] set. *)
let env_with vars =
  let kept s =
    match String.index_opt s '=' with Some i -> not (List.mem_assoc (String.sub s 0 i) vars) | None -> true
  in
  Array.of_list (List.map (fun (k, v) -> k ^ "=" ^ v) vars @ List.filter kept (Array.to_list (Unix.environment ())))

let rec wait pid deadline =
  match Unix.waitpid (if deadline = infinity then [] else [ WNOHANG ]) pid with
  | 0, _ when Unix.gettimeofday () > deadline ->
      Unix.kill pid Sys.sigkill;
      ignore (Unix.waitpid [] pid);
      None
  | 0, _ ->
      Unix.sleepf 0.1;
      wait pid deadline
  | _, WEXITED code -> Some code
  | _, (WSIGNALED _ | WSTOPPED _) -> Some 255

(** [run prog args] runs [prog] in [cwd] and returns its exit code ([None] when it was killed after [timeout] seconds),
    its stdout and its stderr. The output goes through files, so no pipe can fill up and block it. *)
let run ?(cwd = ".") ?(env = Unix.environment ()) ?(timeout = infinity) prog args =
  let out = Filename.temp_file "gates" ".out" and err = Filename.temp_file "gates" ".err" in
  let o = Unix.openfile out [ O_WRONLY; O_CLOEXEC ] 0 and e = Unix.openfile err [ O_WRONLY; O_CLOEXEC ] 0 in
  let child () =
    Unix.chdir cwd;
    Unix.dup2 ~cloexec:false o Unix.stdout;
    Unix.dup2 ~cloexec:false e Unix.stderr;
    Unix.execvpe prog (Array.of_list (prog :: args)) env
  in
  let pid = match Unix.fork () with 0 -> ( try child () with _ -> Unix._exit 127) | pid -> pid in
  List.iter Unix.close [ o; e ];
  let code = wait pid (Unix.gettimeofday () +. timeout) in
  let result = (code, slurp out, slurp err) in
  List.iter Sys.remove [ out; err ];
  result

let git ?env args =
  match run ?env "git" args with
  | Some 0, out, _ -> out
  | _, _, err ->
      fail "git %s failed (a missing ref may need `git fetch origin`): %s" (String.concat " " args) (String.trim err)

(** Where a file is read: at a commit, or on disk as it is now. *)
type rev = At of string | Disk

let is_file p = Sys.file_exists p && not (Sys.is_directory p)

(** The file [path] at [rev], or [None] when it has none. *)
let read rev path =
  match rev with
  | At r -> ( match run "git" [ "show"; r ^ ":" ^ path ] with Some 0, text, _ -> Some text | _ -> None)
  | Disk -> if is_file path then Some (slurp path) else None

let text rev path = Option.value (read rev path) ~default:""

(** The files under [folder] at [rev]; on disk, those git tracks or does not ignore. *)
let paths rev folder =
  match rev with
  | At r -> split '\000' (git [ "ls-tree"; "-r"; "-z"; "--name-only"; r; "--"; folder ])
  | Disk ->
      let listed = git [ "ls-files"; "-z"; "--cached"; "--others"; "--exclude-standard"; "--"; folder ] in
      List.sort_uniq compare (List.filter is_file (split '\000' listed))

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then (
    mkdir_p (Filename.dirname dir);
    Sys.mkdir dir 0o755)

(** Make [path] hold [text], or delete it for [None]. *)
let write path = function
  | None -> if Sys.file_exists path then Sys.remove path
  | Some text ->
      mkdir_p (Filename.dirname path);
      Out_channel.with_open_bin path (fun oc -> output_string oc text)

let with_temp_dir f =
  let dir = Filename.temp_dir "gates" "" in
  Fun.protect ~finally:(fun () -> ignore (run "rm" [ "-rf"; dir ])) (fun () -> f dir)

(** The lines of [path] at [sha] that differ from [parent]; a deletion touches the lines on either side of it. *)
let touched parent sha path =
  let hunk = Re.Perl.compile_pat ~opts:[ `Multiline ] {|^@@ -\S+ \+(\d+)(,(\d+))? @@|} in
  let lines g =
    let start = int_of_string (Re.Group.get g 1) in
    match Re.Group.get_opt g 3 with
    | Some "0" -> [ start; start + 1 ]
    | Some n -> List.init (int_of_string n) (( + ) start)
    | None -> [ start ]
  in
  List.concat_map lines (Re.all hunk (git [ "diff"; "-U0"; "--no-renames"; parent; sha; "--"; path ]))

type report = { title : string; rows : string list; failed : int }
(** A Markdown table under a title, and how many of its rows failed. *)

(** Print [r], and append it to the GitHub step summary when there is one. *)
let print r =
  let text = Printf.sprintf "### %s\n\n%s\n" r.title (String.concat "\n" r.rows) in
  let append path =
    Out_channel.with_open_gen [ Open_append; Open_creat; Open_wronly ] 0o644 path (fun oc -> output_string oc text)
  in
  print_endline text;
  Option.iter append (Sys.getenv_opt "GITHUB_STEP_SUMMARY")
