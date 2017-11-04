open Core

open Term

type t = Environment.t * Location.Range.t

exception NoMatch

let all template source =
  Sequence.empty

let loc =
  let open Location in
  { Range.start_ = { line = 0; column = 0 }
  ; Range.end_ = { line = 0; column = 0 }
  }


(** Helper function to add a single term to a Var during matching *)
let add_term env v term =
  match Environment.lookup env v with
  | Compound ("block", existing_terms) ->
    (* Var is a block, so append the new term *)
    let matches = Compound ("block", existing_terms @ [term]) in
    Environment.add env v matches
  | Var _ ->
    (* Var does not exist, so add a term and continue *)
    Environment.add env v term
  (* var has only been matched with one term, extend it to be a compound. and
     continue *)
  | existing_term ->
    let matches = Compound ("block", existing_term::[term]) in
    Environment.add env v matches


(** Helper function to add a multiple terms to a Var during matching *)
let add_terms env v terms =
  match Environment.lookup env v with
  | Compound ("block", existing_terms) ->
    (* Var is a block, so append the term with hole and stop *)
    let matches = Compound ("block", existing_terms @ terms) in
    Environment.add env v matches
  | Var _ ->
    (* Var does not exist, so add a block term and stop *)
    let term = Compound ("block", terms) in
    Environment.add env v term
  (* var has only been matched with one term, extend it to be a
     compound. and continue *)
  | existing_term ->
    let matches = Compound ("block", existing_term::terms) in
    Environment.add env v matches

let rec skip_until_not_white = function
  | White _::tl -> skip_until_not_white tl
  | x -> x


let rec find_aux env template source : (Environment.t * Location.Range.t) =
  match template, source with
  | Const c1, Const c2 when c1 = c2 -> env, loc
  | White _, White _ -> env, loc
  | Break, Break -> env, loc
  | Compound ("block", lhs), Compound ("block", rhs) ->
    find_list env lhs rhs
  | Compound (c1, [b1]), Compound(c2, [b2]) when c1 = c2 ->
    let env, _ = find_aux env b1 b2 in
    env, loc
  | Var v, term ->
    (Environment.add env v term), loc
  | _, _ ->
    raise NoMatch

and find_list env lhs rhs =
  (*Format.printf "Matching Sz %d %s@.\
                 With     Sz %d %s@.@."
    (List.length lhs)
    (Term.to_string (Compound ("debug", lhs)))
    (List.length rhs)
    (Term.to_string (Compound ("debug", rhs)));*)

  match lhs, rhs with
  | White _::lhs_tl, rhs ->
    find_list env lhs_tl rhs

  | lhs, White _::rhs_tl ->
    find_list env lhs rhs_tl

  | Break::lhs_tl, Break::rhs_tl ->
    find_list env lhs_tl rhs_tl

  | Const c1::lhs_tl, Const c2::rhs_tl when c1 = c2 ->
    find_list env lhs_tl rhs_tl

  (* Identify start of matching, Part 1: when it is a const before hole*)
  | Const c1::(Var v::lhs_tl as lhs_continue_match),
    Const c2::rhs_tl
    when c1 = c2 ->
    begin match skip_until_not_white rhs_tl with
      | start::rhs_tl ->
        let env = Environment.add env v start in
        find_list env lhs_continue_match rhs_tl
      | [] -> env, loc (* Var associates with nothing, end of the list *)
    end

  (* Identify start of matching, Part 2: when there is whitespace *)
  | Const c1::White _::(Var v::lhs_tl as lhs_continue_match),
    Const c2::rhs_tl
    when c1 = c2 ->
    begin match skip_until_not_white rhs_tl with
      | start::rhs_tl ->
        let env = Environment.add env v start in
        find_list env lhs_continue_match rhs_tl
      | [] -> env, loc (* Var associates with nothing, end of the list *)
    end

  | Var v::suffix::rest as lhs_continue_match,
    term::rhs_tl ->

    begin match suffix, term with
      | Compound (c1, terms_lhs), Compound (c2, terms_rhs)
        when c1 = c2 ->
        let env,_ = find_list env terms_lhs terms_rhs in (* XXX loc *)
        (* do not continue with var. we matched a suffix. we are done. we also
           processed everything inside suffix, so we are done there too *)
        find_list env rest rhs_tl
      | Const c1, Const c2 when c1 = c2 ->
        (* we are done with this var, and suffix. continue with rest,rhs_tl *)
        find_list env rest rhs_tl
      (* if suffix is whitespace, we need to trim it and continue and try again *)
      | White _, term ->
        find_list env (Var v::rest) (term::rhs_tl)
      (* else, not equal, then add term (including whitespace) and continue *)
      | _ ->
        let env = add_term env v term in
        (* XXX fix up loc in add_term *)
        find_list env lhs_continue_match rhs_tl
    end

  (* We kept consuming and adding terms to var's list, and reached the end of
     source. Just return env *)
  | [Var _], [] ->
    env, loc

  | [Var v], [term] ->
    let env = add_term env v term in (* XXX fix up loc *)
    env, loc

  | [Var v], last_terms ->
    let env = add_terms env v last_terms in
    env, loc (* XXX fix up loc *)

  | Compound ("block", lhs)::lhs_tl, Compound ("block", rhs)::rhs_tl ->
    let env,_ = find_list env lhs rhs in (* XXX take care of loc *)
    find_list env lhs_tl rhs_tl

  | Compound (c1, [b1])::lhs_tl, Compound (c2, [b2])::rhs_tl ->
    let env,_ = find_aux env b1 b2 in (* XXX take care of loc *)
    find_list env lhs_tl rhs_tl

  | Compound (c1, [])::lhs_tl, Compound (c2, [])::rhs_tl ->
    find_list env lhs_tl rhs_tl

  | [], _ ->
    env, loc

  | _, _ ->
    raise NoMatch


let find template source =
  let env = Environment.create () in
  try Some (find_aux env template source)
  with _ -> None

let exists template source = false

let to_string match_ = ""
