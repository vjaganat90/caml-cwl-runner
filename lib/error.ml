include Data.Error

let ( let* ) = Result.bind

let rec map_list f = function
  | [] -> Ok []
  | x :: xs ->
      let* y = f x in
      let* ys = map_list f xs in
      Ok (y :: ys)

let unimplemented ?(in_requirements = false) ~feature json_path =
  {
    feature;
    json_path;
    in_requirements;
    message =
      (if in_requirements then
         Printf.sprintf "%s is not implemented (requirement)" feature
       else Printf.sprintf "%s is not implemented" feature);
  }

let pp fmt = function
  | Parse { path; message } -> (
      match path with
      | None -> Format.fprintf fmt "parse error: %s" message
      | Some p -> Format.fprintf fmt "parse error in %s: %s" p message)
  | Schema { path; message } ->
      Format.fprintf fmt "schema error at %s: %s" path message
  | Unsupported { feature } ->
      Format.fprintf fmt "unsupported feature: %s" feature
  | Type { param; expected; got } ->
      Format.fprintf fmt "type error for %s: expected %s, got %s" param expected
        got
  | Expr { message } -> Format.fprintf fmt "expression error: %s" message
  | Missing { param } ->
      Format.fprintf fmt "missing required input parameter '%s'" param

let to_string t = Format.asprintf "%a" pp t

let pp_diagnostic fmt d =
  if d.in_requirements then
    Format.fprintf fmt "%s at %s (requirement): %s" d.feature d.json_path
      d.message
  else Format.fprintf fmt "%s at %s: %s" d.feature d.json_path d.message

let diagnostic_to_string d = Format.asprintf "%a" pp_diagnostic d
