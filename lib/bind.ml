let ( let* ) = Error.( let* )

type key_atom = I of int | S of string
type sort_key = key_atom list
type payload = Value of Ty.value | Prefix_only of string

type item = {
  key : sort_key;
  binding : Ty.binding;
  payload : payload;
  name : string;
}

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

let append_pos key n = key @ [ I n ]
let append_name key name = key @ [ S name ]
let append_index key i = key @ [ I i ]
let empty_binding = Ty.default_binding
let is_argument name = String.starts_with ~prefix:"arguments" name

let argv (module E : Expr.ENGINE) ~tool ~inputs ~runtime =
  let ctx0 = { Expr.inputs; self = Ty.Vnull; runtime } in
  let resolve_position ctx = function
    | Ty.Pos n -> Ok n
    | Ty.Expr s -> (
        match E.eval ~ctx ~expr:s with
        | Ok (Ty.Vint n) -> Ok (Int64.to_int n)
        | Ok Ty.Vnull -> Ok 0
        | Ok v ->
            Error
              (Error.Expr
                 {
                   message =
                     Printf.sprintf "position expression must yield int, got %s"
                       (Ty.value_kind v);
                 })
        | Error _ as e -> e)
  in
  let rec collect ctx ~lead ~name ~ty ~binding_opt value acc =
    match value with
    | Ty.Vnull -> Ok acc
    | _ -> (
        match ty with
        | Ty.Union ts ->
            let rec pick = function
              | [] ->
                  collect ctx ~lead ~name ~ty:Ty.String ~binding_opt value acc
              | t :: rest ->
                  let matches =
                    match (t, value) with
                    | Ty.Null, Ty.Vnull -> true
                    | Ty.Boolean, Ty.Vbool _ -> true
                    | (Ty.Int | Ty.Long), Ty.Vint _ -> true
                    | (Ty.Float | Ty.Double), (Ty.Vfloat _ | Ty.Vint _) -> true
                    | Ty.String, Ty.Vstring _ -> true
                    | Ty.File, Ty.Vfile _ -> true
                    | Ty.Directory, Ty.Vdir _ -> true
                    | Ty.Array _, Ty.Varray _ -> true
                    | _ -> false
                  in
                  if matches then
                    collect ctx ~lead ~name ~ty:t ~binding_opt value acc
                  else pick rest
            in
            pick ts
        | Ty.Array { items; item_binding } ->
            let xs = match value with Ty.Varray xs -> xs | _ -> [ value ] in
            let* parent_key =
              match binding_opt with
              | None -> Ok lead
              | Some b ->
                  let* p = resolve_position ctx b.Ty.position in
                  Ok (append_pos lead p)
            in
            let parent_key = append_name parent_key name in
            let acc =
              match binding_opt with
              | Some b when b.Ty.item_separator <> None ->
                  { key = parent_key; binding = b; payload = Value value; name }
                  :: acc
              | Some b -> (
                  match b.Ty.prefix with
                  | Some p ->
                      {
                        key = parent_key;
                        binding = b;
                        payload = Prefix_only p;
                        name;
                      }
                      :: acc
                  | None -> acc)
              | None -> acc
            in
            if
              match binding_opt with
              | Some b when b.Ty.item_separator <> None -> true
              | _ -> false
            then Ok acc
            else
              let item_b =
                match item_binding with
                | Some b -> Some b
                | None -> Some empty_binding
              in
              let rec go i acc = function
                | [] -> Ok acc
                | x :: xs ->
                    let lead = append_index parent_key i in
                    let* acc =
                      collect ctx ~lead
                        ~name:(name ^ "[" ^ string_of_int i ^ "]")
                        ~ty:items ~binding_opt:item_b x acc
                    in
                    go (i + 1) acc xs
              in
              go 0 acc xs
        | _ -> (
            match binding_opt with
            | None -> Ok acc
            | Some b ->
                let* p = resolve_position ctx b.Ty.position in
                let key = append_name (append_pos lead p) name in
                Ok ({ key; binding = b; payload = Value value; name } :: acc)))
  in
  let rec collect_arguments ctx i acc = function
    | [] -> Ok acc
    | Schema.Literal s :: rest ->
        let item =
          {
            key = [ I 0; I i ];
            binding = empty_binding;
            payload = Value (Ty.Vstring s);
            name = Printf.sprintf "arguments[%d]" i;
          }
        in
        collect_arguments ctx (i + 1) (item :: acc) rest
    | Schema.Binding b :: rest ->
        let* p = resolve_position ctx b.Ty.position in
        let item =
          {
            key = [ I p; I i ];
            binding = b;
            payload = Value Ty.Vnull;
            name = Printf.sprintf "arguments[%d]" i;
          }
        in
        collect_arguments ctx (i + 1) (item :: acc) rest
  in
  let rec collect_inputs ctx acc = function
    | [] -> Ok acc
    | (inp : Schema.input) :: rest ->
        let value =
          match Ty.lookup inp.id inputs with Some v -> v | None -> Ty.Vnull
        in
        let ctx = { ctx with Expr.self = value } in
        let* acc =
          collect ctx ~lead:[] ~name:inp.id ~ty:inp.ty
            ~binding_opt:inp.input_binding value acc
        in
        collect_inputs ctx acc rest
  in
  let apply_value_from ctx item =
    match (item.payload, item.binding.value_from) with
    | Prefix_only _, _ | Value _, None -> Ok item
    | Value Ty.Vnull, Some _ when not (is_argument item.name) ->
        Ok { item with payload = Value Ty.Vnull }
    | Value v, Some expr -> (
        let ctx = { ctx with Expr.self = v } in
        match E.eval ~ctx ~expr with
        | Error _ as e -> e
        | Ok v -> Ok { item with payload = Value v })
  in
  let flag = function Some p -> [ p ] | None -> [] in
  let tokens ~separate prefix values =
    match (prefix, values) with
    | _, [] -> []
    | None, vs -> vs
    | Some p, v :: vs when not separate -> (p ^ v) :: vs
    | Some p, vs -> p :: vs
  in
  let generate_arg item =
    match item.payload with
    | Prefix_only p -> [ p ]
    | Value Ty.Vnull | Value (Ty.Vbool false) -> []
    | Value (Ty.Vbool true) | Value (Ty.Vrecord _) -> flag item.binding.prefix
    | Value (Ty.Varray xs) -> (
        match item.binding.item_separator with
        | Some sep when xs <> [] ->
            tokens ~separate:item.binding.separate item.binding.prefix
              [ String.concat sep (List.map Ty.string_of_value xs) ]
        | _ -> if xs = [] then [] else flag item.binding.prefix)
    | Value v ->
        tokens ~separate:item.binding.separate item.binding.prefix
          [ Ty.string_of_value v ]
  in
  let* acc = collect_arguments ctx0 0 [] tool.Schema.arguments in
  let* items = collect_inputs ctx0 acc tool.Schema.inputs in
  let rec eval_all acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs ->
        let* x = apply_value_from ctx0 x in
        eval_all (x :: acc) xs
  in
  let* items = eval_all [] items in
  let items = List.sort cmp_item items in
  let args = List.concat_map generate_arg items in
  Ok (tool.Schema.base_command @ args)
