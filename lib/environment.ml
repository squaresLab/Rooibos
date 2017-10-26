open Core_kernel
open Term

type t = (variable, Term.t) List.Assoc.t

let create () =
  []

let add env var term =
  (var,term) :: env

let rec lookup (env : t) (x : variable) : Term.t =
  match List.Assoc.find ~equal:Pervasives.(=) env x with
  | Some x -> x
  | None -> Var x

let rec substitute env = function
  | Var x as e ->
    (let e' = lookup env x in
     if e = e' then e' else substitute env e')
  | Const _ | Break | Separator _ as e -> e
  | Compound (c, ls) -> Compound (c, List.map ~f:(substitute env) ls)

let to_string env =
    match List.filter ~f:(fun ((_, n), _) -> n = 0) env with
  | [] -> "Yes"
  | env' -> String.concat ~sep:"\n"
              (List.map
                 ~f:(fun ((x,n), e) ->
                    x ^ " = " ^ to_string (substitute env e))
                 (List.rev env'))
