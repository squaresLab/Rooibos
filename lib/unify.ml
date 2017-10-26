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


(** 'Normalize' a term by flattening consts and vars and returning the strings:
    one top-level compound "block" (i.e., list) is allowed, but no other nested
    compounds. Unifying nested compounds should already happen before hitting
    this stage.  (Uh, this may turn out to be possibly wrong) *)
let normalize : Term.t -> Term.t list =
  function
  | Const _ as c -> [c]
  | Var _ as v -> [v]
  | Compound ("block",l) ->
    List.filter l ~f:(function | Compound _ -> false | t -> true)
  | Compound _ -> failwith "Refusing to normalize top-level delimiters"

let concat_const =
  List.fold ~init:"" ~f:(fun acc -> function
      | Const c -> c^acc
      | _ -> failwith "Only consts allowed")

let get_result solver =
  match Smtlib.check_sat solver with
  | Sat -> Smtlib.get_model solver
  | _ -> failwith "Cannot get sat"

let do_decls solver holes =
  List.iter holes ~f:(fun id ->
      Smtlib.declare_const solver (Id id) (Sort (Id "String")))

(** Only allow holes in t1 for now. This is not validated yet *)
let unify_flat env lhs rhs =
  let lhs = normalize lhs in
  let rhs = normalize rhs in
  let holes = List.filter_map lhs ~f:(function | Var (s,_) -> Some s | _ -> None) in
  let rhs = Smtlib.QString (concat_const rhs) in
  let lhs =
    List.map lhs ~f:(function
        | Const c -> Smtlib.QString c
        | Var (v,_) -> Smtlib.Const (Id v)
        | Compound _ -> failwith "Not allowed")
  in

  let solve assertions =
    (* construct z3 calls *)
    let solver = make_solver solver_path in
    (* Add holes *)
    do_decls solver holes;
    (* Add assertions *)
    List.iter assertions ~f:(fun term -> Smtlib.assert_ solver term);
    (* run it *)
    get_result solver
  in

  let results : (Smtlib.identifier * Smtlib.term) list list =
    let rec aux
        (assertions : Smtlib.term list)
        (acc : (Smtlib.identifier * Smtlib.term) list list) =
      try
        let result = solve assertions in
        let negation : Smtlib.term =
          App (Id "and",
               List.map result ~f:(fun (Id id,term) ->
                   match term with
                   | String s -> Smtlib.(not_ (equals (Const (Id id)) (QString s)))
                   | _ ->
                     failwith
                     @@ sprintf "id %s is a non-string term I didn't expect" id)
              )
        in
        aux (negation::assertions) (result::acc)
      with _ -> acc
    in
    aux [(Smtlib.equals (Smtlib.Str.concat lhs) rhs)] []
  in
  List.iter results ~f:(fun result ->
      List.iter result ~f:(fun (Id id, term) ->
          match term with
          | String s ->
            Format.printf "id: %s = %s@." id s;
          | _ ->
            Format.printf "id %s is a non-string term I didn't expect" id));
  env
