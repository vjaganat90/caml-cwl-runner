(** The JSON files the gate reads: [gates.json], with the limits, and the mutants under [test/mutants/]. A file of the
    wrong shape stops the gate. *)

open Repo

type lines = { code : int; tests : int }
(** Lines of code and of tests: a budget, or what a commit adds. *)

type t = { budgets : (string * lines) list; window : int; max_function : int; top_level : string list }

let field k = function
  | `O kv -> ( match List.assoc_opt k kv with Some v -> v | None -> fail "it has no %S" k)
  | _ -> fail "it is not an object"

let int k v = match field k v with `Float f -> int_of_float f | _ -> fail "%S is not a number" k
let string k v = match field k v with `String s -> s | _ -> fail "%S is not a string" k

(** [f] of the JSON [text] of the file [name]. *)
let decode name f text =
  match Yaml.of_string text with
  | Error (`Msg m) -> fail "%s is not JSON: %s" name m
  | Ok v -> ( try f v with Fail m -> fail "%s: %s" name m)

let of_string =
  let budget = function k, b -> (k, { code = int "code" b; tests = int "tests" b }) in
  let top = function `String s -> s | _ -> fail "top_level holds a non-string" in
  let limits v =
    match (field "budgets" v, field "top_level" v) with
    | `O budgets, `A tops ->
        {
          budgets = List.map budget budgets;
          window = int "window" v;
          max_function = int "max_function" v;
          top_level = List.map top tops;
        }
    | _ -> fail "budgets is not an object, or top_level not a list"
  in
  decode "gates.json" limits

(** The limits at [rev], or on disk when [rev] has no gates.json. *)
let at rev =
  match (read rev "gates.json", read Disk "gates.json") with
  | Some text, _ | None, Some text -> of_string text
  | None, None -> fail "there is no gates.json"

(** The budget of a commit of type [kind]: [other] when no budget names it. *)
let budget t kind =
  match (List.assoc_opt (Option.value kind ~default:"other") t.budgets, List.assoc_opt "other" t.budgets) with
  | Some b, _ | None, Some b -> b
  | None, None -> fail "gates.json: budgets has no \"other\""

type mutant = { file : string; old : string; fresh : string; why : string }
(** A file holding [old] exactly once; [fresh] replaces it and breaks [why]. *)

(** The mutants at [rev]. *)
let mutants rev =
  let mutant v = { file = string "file" v; old = string "old" v; fresh = string "new" v; why = string "why" v } in
  List.map
    (fun p -> decode (p ^ " is not a mutant {\"file\", \"old\", \"new\", \"why\"}") mutant (text rev p))
    (paths rev "test/mutants/")

let key m = (m.file, m.old, m.fresh)
