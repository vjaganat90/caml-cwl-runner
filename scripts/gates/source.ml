(** OCaml source, read with the compiler's own lexer and parser: its code lines and their shapes, its literals, and what
    its syntax tree says about definitions, vals and the identifiers that pure code may not use. A source that does not
    lex or parse reads as empty here, and [problem] says why. *)

open Parsetree

let () = ignore (Warnings.parse_options false "-a")

type token = { tok : Parser.token; text : string; first : int; last : int }
(** A token other than a comment, with the lines it spans. *)

type comment = { body : string; from : int; upto : int }

let ocaml p = List.exists (Filename.check_suffix p) [ ".ml"; ".mli" ]
let line (p : Lexing.position) = p.pos_lnum
let range a b = List.init (max 0 (b - a + 1)) (( + ) a)

(** The tokens and comments of [src], or [Error ()] when it does not lex. *)
let lex src =
  Lexer.init ();
  let lb = Lexing.from_string src in
  let note body (l : Location.t) = { body; from = line l.loc_start; upto = line l.loc_end } in
  let rec go toks coms =
    match Lexer.token_with_comments lb with
    | Parser.EOF -> Ok (List.rev toks, List.rev coms)
    | EOL -> go toks coms
    | COMMENT (body, l) -> go toks (note body l :: coms)
    | DOCSTRING d -> go toks (note (Docstrings.docstring_body d) (Docstrings.docstring_loc d) :: coms)
    | tok ->
        let a, b = (lb.lex_start_p, lb.lex_curr_p) in
        let text = String.sub src a.pos_cnum (b.pos_cnum - a.pos_cnum) in
        go ({ tok; text; first = line a; last = line b } :: toks) coms
  in
  try go [] [] with Lexer.Error _ -> Error ()

let tokens src = match lex src with Ok (t, _) -> t | Error () -> []
let comments src = match lex src with Ok (_, c) -> c | Error () -> []

(** A token with its names and literals erased, so that code differing only in those has the same shape. *)
let shape t =
  match t.tok with
  | LIDENT _ -> "x"
  | UIDENT _ -> "C"
  | INT _ | FLOAT _ -> "0"
  | STRING _ | CHAR _ | QUOTED_STRING_EXPR _ | QUOTED_STRING_ITEM _ -> {|"s"|}
  | LABEL _ -> "~x:"
  | OPTLABEL _ -> "?x:"
  | _ -> t.text

(** (line, shape) of each line of [src] that holds code: a line that is not blank and that a token other than a comment
    covers. A token spanning lines gives its shape to its first line. *)
let code_lines src =
  let text = Array.of_list (String.split_on_char '\n' src) in
  let pieces t = (t.first, shape t) :: List.map (fun n -> (n, "")) (range (t.first + 1) t.last) in
  let group acc (n, s) = match acc with (m, ss) :: rest when m = n -> (m, s :: ss) :: rest | _ -> (n, [ s ]) :: acc in
  List.fold_left group [] (List.concat_map pieces (tokens src))
  |> List.rev_map (fun (n, ss) -> (n, String.concat " " (List.rev (List.filter (( <> ) "") ss))))
  |> List.filter (fun (n, _) -> String.trim text.(n - 1) <> "")

(** The tokens of [src] as text: equal when only comments and layout differ. A source that does not lex is its own text.
*)
let words src = match lex src with Ok (t, _) -> List.map (fun t -> t.text) t | Error () -> [ src ]

let literal t =
  match t.tok with
  | STRING _ | CHAR _ | INT _ | FLOAT _ | TRUE | FALSE | QUOTED_STRING_EXPR _ | QUOTED_STRING_ITEM _ -> true
  | _ -> false

(** The literals of [src], sorted: what a test expects. *)
let literals src = List.sort compare (List.map (fun t -> t.text) (List.filter literal (tokens src)))

let parse f src =
  Lexer.init ();
  let lb = Lexing.from_string src in
  try Ok (f lb) with Syntaxerr.Error _ | Lexer.Error _ -> Error (line lb.lex_curr_p)

(** Why the OCaml file [path] holding [src] does not parse, if it does not. *)
let problem path src =
  let check lb =
    if Filename.check_suffix path ".mli" then ignore (Parse.interface lb) else ignore (Parse.implementation lb)
  in
  match parse check src with
  | Ok () -> None
  | Error n -> Some (Printf.sprintf "%s:%d does not parse: fix the syntax" path n)

let raising =
  String.split_on_char ' '
    "raise raise_notrace failwith invalid_arg exit Option.get List.hd List.tl List.nth List.find List.assoc List.assq \
     List.map2 List.iter2 List.fold_left2 List.fold_right2 List.for_all2 List.exists2 List.combine Result.get_ok \
     Result.get_error Hashtbl.find String.sub String.index String.rindex String.index_from Bytes.sub int_of_string \
     float_of_string bool_of_string Int64.of_string Char.chr"

let impure_modules =
  String.split_on_char ' '
    "Sys Unix Eio Eio_unix Eio_main In_channel Out_channel Lwt Hashtbl Obj Queue Stack Atomic Mutex Condition Domain \
     Thread Random"

let impure_names =
  String.split_on_char ' '
    "read_line read_int read_float input_line input_char input_byte output_string output_char output_bytes \
     output_substring flush flush_all at_exit ref incr decr := Printf.printf Printf.eprintf Printf.fprintf \
     Format.printf Format.eprintf Filename.open_temp_file"

let impure_prefixes =
  String.split_on_char ' ' "print_ prerr_ open_in open_out close_in close_out really_input Filename.temp_"

type use = Impure of string | Raises of string  (** What pure code may not do. *)

let classify lid =
  let path = match Longident.flatten lid with "Stdlib" :: p -> p | p -> p in
  let name = String.concat "." path in
  let prefixed = List.exists (fun prefix -> String.starts_with ~prefix name) impure_prefixes in
  match path with
  | m :: _ when List.mem m impure_modules -> Some (Impure m)
  | _ when prefixed || List.mem name impure_names -> Some (Impure name)
  | _ when List.mem name raising -> Some (Raises name)
  | _ -> None

type value = { name : string; doc : string; args : (Asttypes.arg_label * string) list }
(** A [val] of an interface: the doc comment after it, and its arguments with their types printed. *)

type fact =
  | Def of string * int * int  (** A [let] or [and] binding and its lines. *)
  | Val of value
  | Use of int * use  (** I/O, mutable state or a raising call, at a line. *)

let print pp x = Format.asprintf "%a" pp x

let rec arguments t =
  match t.ptyp_desc with
  | Ptyp_arrow (label, arg, rest) -> (label, print Pprintast.core_type arg) :: arguments rest
  | Ptyp_poly (_, t) -> arguments t
  | _ -> []

(** The text of [a] when it is a doc comment that comes after [vd]. *)
let doc_after vd a =
  match a.attr_payload with
  | PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant c; _ }, _); _ } ]
    when a.attr_name.txt = "ocaml.doc" && a.attr_loc.loc_start.pos_cnum >= vd.pval_loc.loc_end.pos_cnum -> (
      match c.pconst_desc with Pconst_string (s, _, _) -> Some s | _ -> None)
  | _ -> None

let value vd =
  let doc = String.concat " " (List.filter_map (doc_after vd) vd.pval_attributes) in
  Val { name = vd.pval_name.txt; doc; args = arguments vd.pval_type }

let definition vb =
  let name = match vb.pvb_pat.ppat_desc with Ppat_var v -> v.txt | _ -> print Pprintast.pattern vb.pvb_pat in
  Def (name, line vb.pvb_loc.loc_start, line vb.pvb_loc.loc_end)

(** Every fact of the syntax tree that [walk] goes through. The iterator's hooks return unit, so what they find collects
    in a ref. *)
let facts walk =
  let found = ref [] and d = Ast_iterator.default_iterator in
  let emit x = found := x :: !found in
  let use (l : Location.t) u = emit (Use (line l.loc_start, u)) in
  let ident ({ txt; loc } : Longident.t Location.loc) = Option.iter (use loc) (classify txt) in
  let expr it e =
    (match e.pexp_desc with
    | Pexp_ident lid -> ident lid
    | Pexp_assert { pexp_desc = Pexp_construct ({ txt = Lident "false"; _ }, None); _ } ->
        use e.pexp_loc (Raises "assert false")
    | Pexp_setfield _ | Pexp_setinstvar _ -> use e.pexp_loc (Impure "<-")
    | _ -> ());
    d.expr it e
  in
  let module_expr it m =
    (match m.pmod_desc with Pmod_ident lid -> ident lid | _ -> ());
    d.module_expr it m
  in
  let typ it t =
    (match t.ptyp_desc with Ptyp_constr (lid, _) -> ident lid | _ -> ());
    d.typ it t
  in
  let label_declaration it l =
    if l.pld_mutable = Mutable then use l.pld_loc (Impure "mutable");
    d.label_declaration it l
  in
  let value_binding it vb =
    emit (definition vb);
    d.value_binding it vb
  in
  let value_description it vd =
    emit (value vd);
    d.value_description it vd
  in
  walk { d with expr; module_expr; typ; label_declaration; value_binding; value_description };
  List.rev !found

let implementation src = facts (fun it -> Result.iter (it.structure it) (parse Parse.implementation src))
let interface src = facts (fun it -> Result.iter (it.signature it) (parse Parse.interface src))

(** (name, code lines) of every [let] or [and] binding in [src], nested ones included. *)
let definitions src =
  let code = List.map fst (code_lines src) in
  let size a b = List.length (List.filter (fun n -> n >= a && n <= b) code) in
  List.filter_map (function Def (name, a, b) -> Some (name, size a b) | _ -> None) (implementation src)

(** Every [val] and [external] of the interface [src], nested ones included. *)
let vals src = List.filter_map (function Val v -> Some v | _ -> None) (interface src)

(** (line, use) of every I/O, mutable state or raising call in [src]. *)
let uses src = List.filter_map (function Use (n, u) -> Some (n, u) | _ -> None) (implementation src)

(** Whether a comment on line [n] of [src], or on the line above, says why a call there cannot raise, in three or more
    words. *)
let says_why src n =
  let why = Re.Perl.compile_pat {|cannot raise:\s*\S+(\s+\S+){2}|} in
  List.exists (fun c -> c.from <= n && c.upto >= n - 1 && Re.execp why c.body) (comments src)
