open Core_kernel
open Environment
open Smtlib
open Term

exception NoUnify

(** Put z3 in your PATH *)
let solver_path = "z3"

(** TODO:
    Add a switch so we can  debug unify with the following code:
    Format.printf "Unify %s@.\
                   With  %s@." (Term.to_string t1) (Term.to_string t2);
*)

let rec unify_terms env t1 t2 =
  match Environment.substitute env t1, Environment.substitute env t2 with
  | t1, t2 when t1 = t2 -> env
  | (Var y, t) | (t, Var y) ->
    if Term.contains t y then
      raise NoUnify
    else
      Environment.add env y t
  | Const _, _ -> raise NoUnify
  | Compound (c1, ts1), Compound (c2, ts2) when c1 = c2 -> unify_lists env ts1 ts2
  | Compound _, _ -> raise NoUnify
and unify_lists env lst1 lst2 =
  try
    List.fold2_exn ~f:(fun env t1 t2 -> unify_terms env t1 t2) ~init:env lst1 lst2
  with Invalid_argument _ -> raise NoUnify


(** 'Normalize' a term by flattening it: one top-level compound
    is allowed, but no other nested compounds. Uh, this is possibly wrong *)
let normalize : Term.t -> Term.t list =
  function
  | Const _ as c -> [c]
  | Var _ as v -> [v]
  | Compound (_,l) ->
    List.filter l ~f:(function
        | Compound _ -> false
        | t -> true)


let concat_const =
  List.fold ~init:"" ~f:(fun acc -> function
      | Const c -> c^acc
      | _ -> failwith "Only consts allowed")

let concat_const_and_var =
  List.fold ~init:"" ~f:(fun acc -> function
      | Const c -> c^acc
      | Var (s,_) -> s^acc
      | _ -> failwith "Only consts and vars allowed")

let get_assertion_result solver (lhs : Smtlib.term list) (rhs : Smtlib.term) =
  Smtlib.assert_ solver (Smtlib.equals (Smtlib.Str.concat lhs) rhs);
  match Smtlib.check_sat solver with
  | Sat -> Smtlib.get_model solver
  | _ -> failwith "Cannot get sat"

(** Only allow holes in t1 for now. This is not validated yet *)
let unify_flat env t1 t2 =
  let solver = make_solver solver_path in
  let t1 = normalize t1 in
  let t2 = normalize t2 in
  let holes_of_term : Term.t list -> string list =
    List.filter_map ~f:(function | Var (s,_) -> Some s | _ -> None) in
  let holes = holes_of_term t1 in
  let add_smt_vars =
    List.iter ~f:(fun id ->
        Smtlib.declare_const solver (Id id) (Sort (Id "String"))) in
  add_smt_vars holes;
  let rhs = Smtlib.QString (concat_const t2) in
  let lhs =
    List.map t1 ~f:(function
      | Const c -> Smtlib.QString c
      | Var (v,_) -> Smtlib.Const (Id v)
      | Compound _ -> failwith "Not allowed")
  in
  let result = get_assertion_result solver lhs rhs in
  List.iter result ~f:(fun (Id id, term) ->
      match term with
      | String s ->
        Format.printf "id: %s = %s@." id s;
      | _ ->
        Format.printf "id %s is a non-string term I didn't expect" id);
  env
