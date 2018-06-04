open Core

open Term

type t = Location.Range.t * Environment.t

exception NoMatch

let range (rng, _) = rng
let environment (_, env) = env

let terms_to_s terms =
  let compound = Compound ("block", terms, Location.Range.unknown) in
    Term.to_string compound

let terms_to_block (terms : Term.t List.t) : Term.t =
  let open Location.Range in
  let range = match terms, (List.rev terms) with
  | (start::_, stop::_) ->
    let { start = start_loc ; _ } = Term.range start in
    let { stop = stop_loc ; _ } = Term.range stop in
      Location.Range.create start_loc stop_loc
  | [], [] ->
      Location.Range.unknown
  | _, _ -> failwith "failed to transform list of terms into a block"
  in
  Compound ("block", terms, range)

(** Helper function to add a single term to a Var during matching *)
let add_term env v term =
  let open Location.Range in
  let { stop = loc_stop; _ } = Term.range term in
  let term' =
  match Environment.lookup env v with
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

(** Helper function to add multiple terms to a Var during matching *)
let add_terms env v terms =
  let open Location.Range in
  let terms' =
  match Environment.lookup env v with
  | Compound ("block", existing_terms, _) -> existing_terms @ terms

  (* Var does not exist, so add a block term and stop *)
  | Var _ -> terms

  (* var has only been matched with one term; extend it to be a
     compound. and continue *)
  | existing_term -> existing_term::terms
  in

  let location =
  match terms', (List.rev terms') with
  | (start::_, stop::_) ->
    let { start = start_loc ; _ } = Term.range start in
    let { stop = stop_loc ; _ } = Term.range stop in
      Location.Range.create start_loc stop_loc
  | _ -> raise NoMatch
  in
  let term' = Compound ("block", terms', location) in
    Environment.add env v term'

let rec skip_until_not_white = function
  | White _::tl -> skip_until_not_white tl
  | x -> x

let rec find_aux env template source : t =
  let range_source = Term.range source in
  (* Format.printf "find_aux: %s\n" (Location.Range.to_string range_source); *)
  match template, source with
  | Const (c1, _), Const (c2, _) when c1 = c2 -> range_source, env
  | Comment (c1, _), Comment (c2, _) when c1 = c2 -> range_source, env
  | White _, White _ -> range_source, env
  | Break _, Break _ -> range_source, env
  | Compound ("block", lhs, _), Compound ("block", rhs, _) ->
    let env, rev_matched_terms = find_list env lhs rhs [] in
    let matched = terms_to_block (List.rev rev_matched_terms) in
    (* Printf.printf "matched list to: %s\n" (Term.to_string_with_loc matched); *)
    (Term.range matched), env
  | Compound (c1, [b1], _), Compound(c2, [b2], _) when c1 = c2 ->
    let _, env = find_aux env b1 b2 in
    range_source, env
  | Var (v, _), term ->
    range_source, (Environment.add env v term)
  | _, _ ->
    raise NoMatch

(* FIXME should return a location range *)
(* acc is used to maintain a buffer of the terms that have been matched *)
and find_list env lhs rhs (acc : Term.t List.t) : Environment.t * Term.t List.t =
  (*
  Format.printf "Matching Sz %d %s@.\
                 With     Sz %d %s@.@."
    (List.length lhs)
    (terms_to_s lhs)
    (List.length rhs)
    (terms_to_s rhs);
  *)
  match lhs, rhs with
  (* FIXME what if the next term in the RHS is also whitespace? *)
  | White _::lhs_tl, rhs ->
    find_list env lhs_tl rhs acc

  (* If the lhs is completely consumed, the rest of rhs doesn't matter. This is
     only true at the first level of matching (no nested compounds). If lhs is
     empty and rhs is not empty for compounds, it is caught in the first case *)
  | [], _ ->
    env, acc

  (* FIXME #30 : by default we want to skip white space on the rhs unless we are
     'in a match'. The problem is, when lhs contains a Var here, we want to keep
     the whitespace when 'in a match', but skip when 'not matching'. Moving the
     case after | Var v::suffix ... doesn't work because the | Var v... will
     bind whitespace before a const, because it is written with the implicit
     assumption that matching the | Var v case means is 'in a match' *)
  | lhs, (White _ as term)::rhs_tl ->
    (* we only accumulate the whitespace if matching has begun *)
    begin
    let acc = match acc with
      | [] -> []
      | _ -> term::acc
    in
      find_list env lhs rhs_tl acc
    end

  | (Break _)::lhs_tl, (Break _ as term)::rhs_tl ->
    find_list env lhs_tl rhs_tl (term::acc)

  | (Const (c1, _))::lhs_tl, (Const (c2, _) as term)::rhs_tl when c1 = c2 ->
    find_list env lhs_tl rhs_tl (term::acc)

  (* Identify start of matching, Part 1: when it is a const before hole *)
  | (Const (c1, _))::((Var (v, _))::lhs_tl as lhs_continue_match),
    (Const (c2, _) as term)::rhs_tl

  (* Identify start of matching, Part 2: when there is whitespace between cons
     and hole *)
  | (Const (c1, _))::White _::((Var (v, _))::lhs_tl as lhs_continue_match),
    (Const (c2, _) as term)::rhs_tl
    when c1 = c2 ->
    begin match skip_until_not_white rhs_tl with
      | start::rhs_tl ->
        let env = Environment.add env v start in
        find_list env lhs_continue_match rhs_tl (term::acc)
      | [] -> env, acc (* Var associates with nothing, end of the list *)
    end

  | (Var (v, loc_var))::suffix::rest as lhs_continue_match,
    term::rhs_tl ->
    begin match suffix, term with
      (* Stop matching condition: an empty lhs compound does not match nonempty rhs *)
      | Compound (c1, [], _), Compound (c2, terms_rhs, _)
        when c1 = c2 && List.length terms_rhs > 0 ->
        raise NoMatch
      (* Stop matching against this var, we matched a suffix. we are done. we
         also processed everything inside suffix, so we are done there too. *)
      | Compound (c1, terms_lhs, _), Compound (c2, terms_rhs, _)
        when c1 = c2 ->
        let env, _ = find_list env terms_lhs terms_rhs [] in
        find_list env rest rhs_tl (term::acc)
      (* we are done with this var, and suffix. continue with the rest *)
      | Const (c1, _), Const (c2, _) when c1 = c2 ->
        find_list env rest rhs_tl (term::acc)
      (* if suffix is whitespace, we need to trim it and continue and try again.
         we know that we will hit some sort of suffix later. therefore it is ok
         to deicide to save term here regardless. we don't want to propagate
         term forward because if it is white space it will be removed. while
         'busy matching', we don't want white space to skip. when not matching,
         we skip. I.e., term may be white space here, and we keep it. *)
      | White _, term ->
        let env = add_term env v term in
        find_list env (Var (v, loc_var)::rest) rhs_tl (term::acc) (* FIXME? *)
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
              | hd::_ when Term.equivalent hd suffix ->
                let env = add_term env v term in
                find_list env lhs_continue_match rhs_tl (term::acc)
              (* add the term and the 'next' white space since it is not suffix *)
              | _ ->
                let env = add_term env v term in
                let env = add_term env v next in
                find_list env lhs_continue_match rhs_tl (term::acc)
            end
          (* if other next term, add this term and continue. other will be
             handled in the next round of the loop*)
          | non_white_next_term::_ ->
            let env = add_term env v term in
            find_list env lhs_continue_match rhs_tl (term::acc)
          | [] -> find_list env lhs_continue_match rhs_tl (term::acc)
        end
    end

  (* We kept consuming and adding terms to var's list, and reached the end of
     source. Just return env *)
  | [Var _], [] ->
    env, acc

  | [Var (v, _)], [term] ->
    (add_term env v term), (term::acc)

  | [Var (v, _)], last_terms ->
    (add_terms env v last_terms), ((List.rev last_terms) @ acc)

  | Compound ("block", lhs, _)::lhs_tl, (Compound ("block", rhs, _) as term)::rhs_tl ->
    let env, _ = find_list env lhs rhs [] in
    find_list env lhs_tl rhs_tl (term::acc)

  | Compound (c1, [b1], _)::lhs_tl, (Compound (c2, [b2], _) as term)::rhs_tl ->
    let _, env = find_aux env b1 b2 in
    find_list env lhs_tl rhs_tl (term::acc)

  | Compound (c1, [], _)::lhs_tl, Compound (c2, [], _)::rhs_tl ->
    find_list env lhs_tl rhs_tl []

  | _, _ ->
    raise NoMatch


let find template source =
  let env = Environment.create () in
  try Some (find_aux env template source)
  with _ -> None


(** shift a source by n. n only comes into play for blocks AKA lists *)
let rec shift_source n source : Term.t option =
  let open Location.Range in
  (*
  Printf.printf "shifting by %d terms at %s: %s\n"
    n
    (Term.range source |> to_string)
    (Term.to_string source);
  *)
  assert (n > 0);
  match source with
  | Compound (c, terms, { stop = loc_stop ; _ })
    when c = "block" && (List.length terms > 0) ->
    let terms = List.drop terms n in
    let loc = begin match terms with
      | [] -> Location.Range.unknown
      | first_term::_ ->
        let { start = loc_start; _ } = Term.range first_term in
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
    let result = find_aux (Environment.create ()) template source in
    let rng, env = result in
    let acc = result::acc in
    Format.printf "found match at %s!\n" (Location.Range.to_string rng);
    match Environment.vars env with
    | [] -> continue 1 acc source
    | var::_ -> begin
      let n = Environment.lookup env var |> size in
      continue (1+n) acc source
      end
  with NoMatch -> continue 1 acc source


(**
   (1) For any list of terms, run 'find_shift' to find all matches on that level
   (2) In the list of terms, extract all compounds (that can contain further
   lists/leaf terms)
   (3) For each list of terms in compounds/leafs, also run find_shift.
*)
let all template source =
  (*
  Printf.printf "Finding matches of template: %s\n" (Term.to_string_with_loc template);
  Printf.printf "in source: %s\n" (Term.to_string_with_loc source);
  *)
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
