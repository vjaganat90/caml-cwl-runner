open Error.Syntax

(* A sort key is a list of atoms: [I n] is a position or an array index, [S s]
   a name. As in invocation.md, numbers sort before strings and strings sort
   lexicographically by their UTF-8 bytes. *)
type key_atom = I of int | S of string
type sort_key = key_atom list

(* The words one binding adds, at its sort key. [name] is the input's id, with
   [[i]] appended for item i of an array, and [arguments[i]] for the i-th
   argument. It breaks ties between equal keys. *)
type item = { key : sort_key; name : string; words : string list }

let cmp_atom a b =
  match (a, b) with
  | I x, I y -> Int.compare x y
  | I _, S _ -> -1
  | S _, I _ -> 1
  | S x, S y -> String.compare x y

let rec cmp_key a b =
  match (a, b) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
      let c = cmp_atom x y in
      if c <> 0 then c else cmp_key xs ys

let cmp_item a b =
  let c = cmp_key a.key b.key in
  if c <> 0 then c else String.compare a.name b.name

let append_int key n = key @ [ I n ]
let append_name key name = key @ [ S name ]

let position (module E : Expr.ENGINE) ctx = function
  | Ty.Pos n -> Ok n
  | Ty.Expr s -> (
      match E.eval ctx s with
      | Ok (Untyped_tree.Int n) -> Ok (Int64.to_int n)
      | Ok Untyped_tree.Null -> Ok 0
      | Ok v ->
          Error.expr
            (Printf.sprintf "position expression must yield int, got %s"
               (Ty.value_kind v))
      | Error _ as e -> e)

(* The value [b] binds: [v], or what its [valueFrom] gives with [self] = [v]. *)
let value_from (module E : Expr.ENGINE) ctx (b : Ty.binding) v =
  match b.value_from with
  | None -> Ok v
  | Some expr -> E.eval { ctx with Expr.self = v } expr

let flag = function Some p -> [ p ] | None -> []

let tokens ~separate prefix values =
  match (prefix, values) with
  | _, [] -> []
  | None, vs -> vs
  | Some p, v :: vs when not separate -> (p ^ v) :: vs
  | Some p, vs -> p :: vs

(* The words of [v] bound by [b], by the kind of [v]. The items of an array
   that is not joined by [itemSeparator] are items of their own. *)
let words (b : Ty.binding) v =
  match v with
  | Untyped_tree.Null | Untyped_tree.Bool false -> []
  | Untyped_tree.Object _ when Ty.class_of v = None -> flag b.prefix
  | Untyped_tree.Bool true -> flag b.prefix
  | Untyped_tree.Array [] -> []
  | Untyped_tree.Array xs -> (
      match b.item_separator with
      | Some sep ->
          tokens ~separate:b.separate b.prefix
            [ String.concat sep (List.map Ty.string_of_value xs) ]
      | None -> flag b.prefix)
  | Untyped_tree.Int _ | Untyped_tree.Float _ | Untyped_tree.String _
  | Untyped_tree.Object _ ->
      tokens ~separate:b.separate b.prefix [ Ty.string_of_value v ]

(* The first member of a union that [value] matches, else [string]. *)
let rec member ts value =
  match ts with
  | [] -> Ty.String
  | t :: rest -> if Ty.matches t value then t else member rest value

(* The items of input [name] of type [ty], bound by [binding] when it has one,
   with value [v]. [lead] is the key of the enclosing array item, if any. *)
let rec bind e ctx lead name ty binding v =
  match (v, ty, binding) with
  | Untyped_tree.Null, _, _ -> Ok []
  | _, Ty.Union ts, _ -> bind e ctx lead name (member ts v) binding v
  | Untyped_tree.Array xs, Ty.Array { items; item_binding = Some ib }, None ->
      elements e ctx (append_name lead name) name items ib xs
  | _, _, None -> Ok []
  | _, _, Some (b : Ty.binding) ->
      let* p = position e ctx b.position in
      let* bound = value_from e ctx b v in
      render e ctx (append_name (append_int lead p) name) name ty b bound

(* The items of [v], the value [b] binds at [key]: its own words, then, for an
   array not joined by [itemSeparator], those of its elements. They are bound
   as the items of [ty] when [v] is the input's own array, else by the default
   binding. *)
and render e ctx key name ty (b : Ty.binding) v =
  let+ tail =
    match (v, ty) with
    | Untyped_tree.Array _, _ when b.item_separator <> None -> Ok []
    | Untyped_tree.Array xs, Ty.Array { items; item_binding }
      when b.value_from = None ->
        elements e ctx key name items
          (Option.value item_binding ~default:Ty.default_binding)
          xs
    | Untyped_tree.Array xs, _ ->
        elements e ctx key name Ty.String Ty.default_binding xs
    | _ -> Ok []
  in
  { key; name; words = words b v } :: tail

(* The items of each element of [xs], each of type [items] and bound by [b],
   after the array's [key]. *)
and elements e ctx key name items b xs =
  let element (i, x) =
    bind e ctx (append_int key i)
      (name ^ "[" ^ string_of_int i ^ "]")
      items (Some b) x
  in
  let+ items = Error.map_list element (List.mapi (fun i x -> (i, x)) xs) in
  List.concat items

(* The items of the [i]-th entry of [arguments]. *)
let argument e ctx (i, (b : Ty.binding)) =
  let* p = position e ctx b.position in
  let* v = value_from e ctx b Untyped_tree.Null in
  render e ctx [ I p; I i ] (Printf.sprintf "arguments[%d]" i) Ty.String b v

let argv e (tool : Command_line_tool.t) inputs runtime =
  let ctx = { Expr.inputs; self = Untyped_tree.Null; runtime } in
  let input (inp : Schema.input) =
    let v =
      Option.value (List.assoc_opt inp.id inputs) ~default:Untyped_tree.Null
    in
    bind e { ctx with self = v } [] inp.id inp.ty inp.input_binding v
  in
  let* args =
    Error.map_list (argument e ctx)
      (List.mapi (fun i a -> (i, a)) tool.arguments)
  in
  let+ inputs = Error.map_list input tool.inputs in
  let items = List.sort cmp_item (List.concat (args @ inputs)) in
  tool.base_command @ List.concat_map (fun item -> item.words) items
