open Core
open Term

type t = (variable, Term.t) List.Assoc.t

let create () =
  []

let vars env =
  List.map env ~f:fst

let add env var term =
  List.Assoc.add ~equal:Pervasives.(=) env var term

let rec lookup (env : t) (var : variable) : Term.t =
  match List.Assoc.find ~equal:Pervasives.(=) env var with
  | Some var -> var
  (* CT: I don't really like this behaviour; I think that [lookup] should
   * return [None] if [var] isn't found. *)
  | None -> Var (var, Location.Range.unknown)

let rec substitute env = function
  | Var (var, _) as e ->
    (let e' = lookup env var in
     if e = e' then e' else substitute env e')
  | White _ | Const _ | Break _ as e -> e
  | Compound (c, ls, loc) ->
      Compound (c, List.map ~f:(substitute env) ls, loc) (* TODO: hacky *)

let strip env =
  let entries =
    List.map ~f:(fun v -> v, (lookup env v)) (vars env)
  in
  let entries =
    List.map ~f:(fun (v, t) -> v, (Term.strip t)) entries
  in
    List.fold ~f:(fun env' (v, t) -> add env' v t) ~init:(create ()) entries

let to_string env =
    match List.filter ~f:(fun ((_, n), _) -> n = 0) env with
  | [] -> "Yes"
  | env' -> String.concat ~sep:"\n"
              (List.map
                 ~f:(fun ((x,n), e) ->
                    x ^ " = " ^ to_string (substitute env e))
                 (List.rev env'))
