open Core

open Term

type t = Environment.t

exception NoMatch
exception EmptyList

(** Helper function to add a single term to a Var during matching *)
let add_term env v term =
  let open Location.Range in
  let { stop = loc_stop; _ } = Term.range term in
  let term' = match Environment.lookup env v with
  (* Var is a block, so append the new term *)
  | Compound ("block", existing_terms, { start = loc_start; _ }) ->
    let loc = Location.Range.create loc_start loc_stop in
    Compound ("block", existing_terms @ [term], loc)

  (* Var does not exist, so add a term and continue *)
  | Var _ -> term

  (* var has only been matched with one term; construct a block from the given
   * term and the existing term. *)
  | existing_term ->
    let { start = loc_start ; _ } = Term.range existing_term in
    let loc = Location.Range.create loc_start loc_stop in
    Compound ("block", existing_term::[term], loc)
  in
    Environment.add env v term'

(** Helper function to add a multiple terms to a Var during matching *)
let add_terms env v terms =
  let open Location.Range in
  let terms' = match Environment.lookup env v with
  (* Var is a block, so append the term with hole and stop *)
  | Compound ("block", existing_terms, _) -> existing_terms @ terms

  (* Var does not exist, so add a block term and stop *)
  | Var _ -> terms

  (* var has only been matched with one term, extend it to be a
     compound. and continue *)
  | existing_term -> existing_term::terms
  in

  (* determine the range of locations covered by the terms bound to the
   * given Var *)
  let loc = match terms', (List.rev terms') with
  | (start::_, stop::_) ->
    let { start = start_loc ; _ } = Term.range start in
    let { stop = stop_loc ; _ } = Term.range stop in
      Location.Range.create start_loc stop_loc
  | _ -> raise EmptyList
  in
  let term' = Compound ("block", terms, loc) in
    Environment.add env v term'

let rec skip_until_not_white = function
  | White _::tl -> skip_until_not_white tl
  | x -> x


let rec find_aux env template source : Environment.t =
  match template, source with
  | Const (c1, _), Const (c2, loc) when c1 = c2 -> env
  | White _, White _ -> env
  | Break _, Break _ -> env
  | Compound ("block", lhs, _), Compound ("block", rhs, _) ->
    find_list env lhs rhs
  | Compound (c1, [b1], _), Compound(c2, [b2], _) when c1 = c2 ->
    find_aux env b1 b2
  | Var (v, _), term ->
    Environment.add env v term
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

  | (Break _)::lhs_tl, (Break _)::rhs_tl ->
    find_list env lhs_tl rhs_tl

  | (Const (c1, _))::lhs_tl, (Const (c2, _))::rhs_tl when c1 = c2 ->
    find_list env lhs_tl rhs_tl

  (* Identify start of matching, Part 1: when it is a const before hole *)
  | (Const (c1, _))::((Var (v, _))::lhs_tl as lhs_continue_match),
    (Const (c2, _))::rhs_tl

  (* Identify start of matching, Part 2: when there is whitespace between cons
     and hole *)
  | (Const (c1, _))::White _::((Var (v, _))::lhs_tl as lhs_continue_match),
    (Const (c2, _))::rhs_tl

    when c1 = c2 ->
    begin match skip_until_not_white rhs_tl with
      | start::rhs_tl ->
        let env = Environment.add env v start in
        find_list env lhs_continue_match rhs_tl
      | [] -> env (* Var associates with nothing, end of the list *)
    end

  | (Var (v, _))::suffix::rest as lhs_continue_match,
    term::rhs_tl ->
    begin match suffix, term with
      (* Stop matching against this var, we matched a suffix. we are done. we
         also processed everything inside suffix, so we are done there too. *)
      | Compound (c1, terms_lhs, _), Compound (c2, terms_rhs, _)
        when c1 = c2 ->
        let env = find_list env terms_lhs terms_rhs in
        find_list env rest rhs_tl
      (* we are done with this var, and suffix. continue with the rest *)
      | Const (c1, _), Const (c2, _) when c1 = c2 ->
        find_list env rest rhs_tl
      (* if suffix is whitespace, we need to trim it and continue and try again.
         we know that we will hit some sort of suffix later. therefore it is ok
         to deicide to save term here regardless. we don't want to propagate
         term forward because if it is white space it will be removed. while
         'busy matching', we don't want white space to skip. when not matching,
         we skip. I.e., term may be white space here, and we keep it. *)
      | White _, term ->
        let env = add_term env v term in
        find_list env (Var (v, Location.Range.unknown)::rest) rhs_tl (* TODO: this is a bit weird *)
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
                find_list env lhs_continue_match rhs_tl
              (* add the term and the 'next' white space since it is not suffix *)
              | _ ->
                let env = add_term env v term in
                let env = add_term env v next in
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
      env

  | [Var (v, _)], [term] ->
    add_term env v term

  | [Var (v, _)], last_terms ->
    add_terms env v last_terms

  | Compound ("block", lhs, _)::lhs_tl, Compound ("block", rhs, _)::rhs_tl ->
    let env = find_list env lhs rhs in
    find_list env lhs_tl rhs_tl

  | Compound (c1, [b1], _)::lhs_tl, Compound (c2, [b2], _)::rhs_tl ->
    let env = find_aux env b1 b2 in
    find_list env lhs_tl rhs_tl

  | Compound (c1, [], _)::lhs_tl, Compound (c2, [], _)::rhs_tl ->
    find_list env lhs_tl rhs_tl

  | [], _ ->
    env

  | _, _ ->
    raise NoMatch


let find template source =
  let env = Environment.create () in
  try Some (find_aux env template source)
  with _ -> None


(** shift a source by n. n only comes into play for blocks AKA lists *)
let rec shift_source n source : Term.t option =
  let open Location.Range in
  assert (n > 0);
  match source with
  | Compound (c, terms, { start = loc_start ; _ })
    when c = "block" && (List.length terms > 0) ->
    let terms = List.drop terms n in
    let loc = begin match List.rev terms with
      | [] -> Location.Range.unknown (* TODO ewww *)
      | last_term::_ ->
        let { stop = loc_stop; _ } = Term.range last_term in
          Location.Range.create loc_start loc_stop
    end in
      Some (Compound (c, terms, loc))
  | _ -> None


let rec bust_out_compounds source : Term.t list =
  match source with
  | Compound ("block", terms, _) ->
    List.filter terms
      ~f:(function | Compound (c, _, _) when c <> "block" -> true | _ -> false)
  (* non-block compounds are always just one element of type block. FIXME #36 *)
  | Compound (c, [terms], _) ->
    bust_out_compounds terms
  | _ -> []


(** A way to find the size of the term for advancing *)
let size (term : Term.t) =
  match term with
  | Compound ("block", terms, _) -> List.length terms
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
    let env = find_aux (Environment.create ()) template source in
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
    | Compound ("block", terms, _) as this_level ->
      let acc = find_shift acc template source in
      let compounds = bust_out_compounds this_level in
      List.fold
        ~init:acc ~f:(fun acc term -> (aux acc template term))
        compounds
    | Compound (c, [block], _) -> aux acc template block
    | _ -> acc
  in
  Sequence.of_list (aux [] template source)


let exists template source =
  try
    all template source |> ignore;
    true
  with | NoMatch -> false
