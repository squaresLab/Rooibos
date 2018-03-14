open Core

open Term

type t = Environment.t

exception NoMatch

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
  | Const "\n"::tl -> skip_until_not_white tl
  | Const "\r\n"::tl -> skip_until_not_white tl
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
  (* Format.printf "Matching Sz %d %s@.\
                 With     Sz %d %s@.@."
    (List.length lhs)
    (Term.to_string (Compound ("debug", lhs)))
    (List.length rhs)
    (Term.to_string (Compound ("debug", rhs))); *)

  match lhs, rhs with
  | White _::lhs_tl, rhs ->
    find_list env lhs_tl rhs

  (* FIXME #30 : by default we want to skip white space on the rhs unless we are
     'in a match'. The problem is, when lhs contains a Var here, we want to keep
     the whitespace when 'in a match', but skip when 'not matching'. Moving the
     case after | Var v::suffix ... doesn't work because the | Var v... will
     bind whitespace before a const, because it is written with the implicit
     assumption that matching the | Var v case means is 'in a match' *)
  | lhs, White _::rhs_tl ->
    find_list env lhs rhs_tl

  | Break::lhs_tl, Break::rhs_tl ->
    find_list env lhs_tl rhs_tl

  | Const c1::lhs_tl, Const c2::rhs_tl when c1 = c2 ->
    find_list env lhs_tl rhs_tl

  (* Identify start of matching, Part 1: when it is a const before hole *)
  | Const c1::(Var v::lhs_tl as lhs_continue_match),
    Const c2::rhs_tl

  (* Identify start of matching, Part 2: when there is whitespace between cons
     and hole *)
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
      (* Stop matching against this var, we matched a suffix. we are done. we
         also processed everything inside suffix, so we are done there too. *)
      | Compound (c1, terms_lhs), Compound (c2, terms_rhs)
        when c1 = c2 ->
        let env,_ = find_list env terms_lhs terms_rhs in (* XXX loc *)
        find_list env rest rhs_tl
      (* we are done with this var, and suffix. continue with the rest *)
      | Const c1, Const c2 when c1 = c2 ->
        find_list env rest rhs_tl
      (* if suffix is whitespace, we need to trim it and continue and try again.
         we know that we will hit some sort of suffix later. therefore it is ok
         to deicide to save term here regardless. we don't want to propagate
         term forward because if it is white space it will be removed. while
         'busy matching', we don't want white space to skip. when not matching,
         we skip. I.e., term may be white space here, and we keep it. *)
      | White _, term ->
        let env = add_term env v term in
        find_list env (Var v::rest) rhs_tl
      (* else, not equal, then add term (including whitespace, if any) and continue *)
      | _, term ->
        begin match rhs_tl with
          (* we want to add whitespace when we are in a match. but:
             we want to not actually add white if the next token
             matches suffix. *)
          | (White _ as next)::rhs_tl ->
            begin match rhs_tl with
              (* don't add whitespace if next token is suffix, and we're at the
                 end *)
              | hd::_ when hd = suffix ->
                let env = add_term env v term in
                (* XXX fix up loc in add_term *)
                find_list env lhs_continue_match rhs_tl
              (* add the term and the 'next' white space since it is not suffix *)
              | _ ->
                let env = add_term env v term in
                let env = add_term env v next in
                (* XXX fix up loc in add_term *)
                find_list env lhs_continue_match rhs_tl
            end
          (* if other next term, add this term and continue. other will be
             handled in the next round of the loop*)
          | non_white_next_term::_ ->
            let env = add_term env v term in
            find_list env lhs_continue_match rhs_tl
          | [] -> find_list env lhs_continue_match rhs_tl
        end
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
  try Some (find_aux env template source |> fst)
  with _ -> None


(** shift a source by n. n only comes into play for blocks AKA lists *)
let rec shift_source n source : Term.t option =
  assert (n > 0);
  match source with
  | Compound (c, terms)
    when c = "block" && (List.length terms > 0) ->
    let terms = List.drop terms n in
    Some (Compound (c, terms))
  | _ -> None


let rec bust_out_compounds source : Term.t list =
  match source with
  | Compound ("block", terms) ->
    List.filter terms
      ~f:(function | Compound (c, _) when c <> "block" -> true | _ -> false)
  (* non-block compounds are always just one element of type block. FIXME #36 *)
  | Compound (c, [terms]) ->
    bust_out_compounds terms
  | _ -> []


(** A way to find the size of the term for advancing *)
let size (term : Term.t) =
  match term with
  | Compound ("block", terms) -> List.length terms
  | _ -> 1


(** Find all matches on this level, shifting. We always shift by 1. If there is
    a match, we calculate the size of the terms in a list matched, and add this
    to 1, to advance past the matched terms. This ensures we do not record
    duplicate matches as we shift. *)
let rec find_shift acc template source =
  let continue n acc source =
    match shift_source n source with
    | Some shifted_term ->
      find_shift acc template shifted_term
    | None -> acc
  in
  try
    let env, _ = find_aux (Environment.create ()) template source in
    let acc = env::acc in
    let var = Environment.vars env |> List.hd_exn in
    let term = Environment.lookup env var in
    let n = size term in
    continue (1+n) acc source
  with | NoMatch -> continue 1 acc source


(**
   (1) For any list of terms, run 'find_shift' to find all matches on that level
   (2) In the list of terms, extract all compounds (that can contain further
   lists/leaf terms)
   (3) For each list of terms in compounds/leafs, also run find_shift.
*)
let all template source =
  let rec aux acc template source =
    match source with
    | Compound ("block", terms) as this_level ->
      let acc = find_shift acc template source in
      let compounds = bust_out_compounds this_level in
      List.fold
        ~init:acc ~f:(fun acc term -> (aux acc template term))
        compounds
    | Compound (c, [block]) -> aux acc template block
    | _ -> acc
  in
  Sequence.of_list (aux [] template source)


let exists template source =
  try
    all template source |> ignore;
    true
  with | NoMatch -> false
