open Core_kernel
open Term

type t = (variable, Term.t Node.t) List.Assoc.t

let create () =
  []

let add env var node =
  List.Assoc.add ~equal:Pervasives.(=) env var node

let rec lookup (env : t) (x : variable) : Term.t Node.t option =
  List.Assoc.find ~equal:Pervasives.(=) env x

let rec substitute env node =
  match Node.term node with
  | Var v ->
    begin
    match lookup env v with
    | None -> node
    | Some(node') -> substitute env node'
    end
  | Compound (c, ls) ->
    let term' = Compound (c, List.map ~f:(substitute env) ls) in
      (* TODO: how should substitution affect locations? *)
      Node.make term' (Node.range node)
  | _ -> node

let to_string env =
  let var_to_string = function
  | (vname, 0) -> vname
  | (vname, vnum) -> vname ^ ":" ^ (Int.to_string vnum)
  in
  let entry_to_string (var, node) =
    (var_to_string var) ^ " -> " ^ (Node.to_string node)
  in
  let l = List.map ~f:entry_to_string env in
    String.concat ~sep:"\n" l
