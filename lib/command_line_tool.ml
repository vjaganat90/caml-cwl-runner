include Data.Command_line_tool
open Error.Annotated

(* The top-level keys of a tool that need no diagnostic: those that name a
   process, Schema Salad's, the process fields and the tool's own. *)
let implemented_tool_keys =
  [ "cwlVersion"; "class"; "id"; "label"; "doc" ]
  @ [ "$namespaces"; "$schemas"; "$base" ]
  @ [ "inputs"; "outputs"; "requirements"; "hints" ]
  @ [ "baseCommand"; "arguments"; "stdin"; "stdout"; "stderr"; "successCodes" ]

let parse_arguments path = function
  | Untyped_tree.Null -> return []
  | Untyped_tree.Array xs ->
      Schema.map_i
        (fun path -> function
          | Untyped_tree.String s ->
              return { Ty.default_binding with value_from = Some s }
          | x -> Schema.parse_binding path x)
        path xs
  | other -> Schema.expected ~json_path:path ~what:"arguments array" other

let parse_success_codes path = function
  | Untyped_tree.Null -> return [ 0 ]
  | (Untyped_tree.Int _ | Untyped_tree.Float _) as v ->
      let+ n = Schema.parse_int path v in
      [ n ]
  | Untyped_tree.Array xs -> Schema.map_i Schema.parse_int path xs
  | other -> Schema.expected ~json_path:path ~what:"int or array of int" other

let of_tree doc =
  match doc with
  | Untyped_tree.Array _ ->
      Error.schema "/" "$graph / packed documents are not implemented"
  | Untyped_tree.Object kvs ->
      let field key = Schema.field ~json_path:"" kvs key in
      let string key =
        Result.bind (Untyped_tree.string_field ~json_path:"" kvs key) return
      in
      let forbidden k =
        if List.mem_assoc k kvs then Error.schema k ("unresolved " ^ k)
        else return ()
      in
      let+ () =
        warn
          (Schema.diagnostics_for_keys ~implemented:implemented_tool_keys
             ~unimplemented:
               [ "intent"; "temporaryFailCodes"; "permanentFailCodes" ]
             "" kvs)
          ()
      and+ () =
        let* class_ = string "class" in
        match Option.value class_ ~default:"" with
        | "CommandLineTool" | "" -> return ()
        | class_ -> warn [ Schema.diag class_ "class" ] ()
      and+ cwl_version = Schema.parse_cwl_version kvs
      and+ _ = all (List.map forbidden [ "$import"; "$include"; "$graph" ])
      and+ base_command =
        field "baseCommand" (Schema.strings ~what:"string") (return [])
      and+ arguments = field "arguments" parse_arguments (return [])
      and+ inputs, outputs, requirements, hints = Schema.parse_process kvs
      and+ stdout = string "stdout"
      and+ stdin = string "stdin"
      and+ stderr = string "stderr"
      and+ success_codes =
        field "successCodes" parse_success_codes (return [ 0 ])
      in
      {
        cwl_version;
        base_command;
        arguments;
        inputs;
        outputs;
        stdout;
        stdin;
        stderr;
        success_codes;
        requirements;
        hints;
      }
  | other -> Schema.expected ~json_path:"/" ~what:"a CWL document object" other

let cores_min tool =
  let from_list rs =
    List.find_map
      (function Schema.Resource { cores_min } -> cores_min | _ -> None)
      rs
  in
  match from_list tool.requirements with
  | Some _ as c -> c
  | None -> from_list tool.hints

let input_specs tool =
  List.map
    (fun (i : Schema.input) -> { Ty.id = i.id; ty = i.ty; default = i.default })
    tool.inputs
