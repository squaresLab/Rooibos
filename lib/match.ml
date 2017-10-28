open Core_kernel.Std

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

let rec find_aux env template source : (Environment.t * Location.Range.t) =
  match template, source with
  | Const c1, Const c2 when c1 = c2 -> env, loc
  | Break, Break -> env, loc
  | Compound ("block", ts1), Compound ("block", ts2) ->
    find_list env ts1 ts2
  | Compound (c1, [b1]), Compound(c2, [b2]) when c1 = c2 ->
    let env, _ = find_aux env b1 b2 in
    env, loc
  | Var ((s,_) as v), term ->
    (Environment.add env v term), loc
  | _, _ ->
    raise NoMatch

and find_list env ts1 ts2 =
  Format.printf "Matching Sz %d %s@.\
                 With     Sz %d %s@.@."
    (List.length ts1)
    (Term.to_string (Compound ("debug", ts1)))
    (List.length ts2)
    (Term.to_string (Compound ("debug", ts2)));

  match ts1, ts2 with
  | Const c1::((Var ((s,_) as v) :: rest1) as continue),
    Const c2::start::rest2
    when c1 = c2 ->
    Format.printf "2. Adding var %s with term %s@." s (Term.to_string start);
    let env = Environment.add env v start in
    find_list env continue rest2

  | (Var ((s,_) as v)::((suffix::rest) as continue) as all),
    ((term::rest2) as continue2) ->
    begin match suffix, term with
      | Compound (c1, terms1), Compound (c2, terms2)
        when c1 = c2 ->
        Format.printf "Same names, processing everything inside brackets@.";
        let env,_ = find_list env terms1 terms2 in (* XXX loc *)
        Format.printf "Done with brackets, continuing with prev list@.";
        (* do not continue with var. we matched a suffix. we are done.  we also
           processed everything inside suffix, so we are done there too*)
        find_list env rest rest2
      | Const c1, Const c2 when c1 = c2 ->
        (* we are done with this var, and suffix. continue with rest,rest2 *)
        find_list env rest rest2
      (* else, not equal, then... *)
      | _ ->
        Format.printf "Not equal, time to add things...@.";
        begin match Environment.lookup env v with
          | Compound ("block", terms) ->
            (* Var is a block, so append the term with hole and continue *)
            let matches = Compound ("block", terms @ [term]) in
            Format.printf "3. Adding var %s with %s@." s (Term.to_string matches);
            let env,_ = (Environment.add env v matches), loc in (* XXX *)
            find_list env continue rest2 (* XXX why is this all *)
          | Var _ ->
            (* Var does not exist, so add a term and continue *)
            Format.printf "4. Adding var %s with %s@." s (Term.to_string term);
            let env,_ = (Environment.add env v term), loc in (* XXX *)
            find_list env all rest2
          (* var has only been matched with one term, extend it to be a
             compound. and continue *)
          | existing_term ->
            Format.printf "Var is currently bound to %s@." (Term.to_string existing_term);
            let matches = Compound ("block", [existing_term; term]) in
            Format.printf "5. Adding var %s with %s@." s (Term.to_string matches);
            let env,_ = (Environment.add env v matches), loc in (* XXX *)
            find_list env all rest2
        end
    end

  (* kept consuming and adding to var, and reach end. just return env *)
  | [Var _], [] ->
    env, loc

  | [Var ((s,_) as v)], [term] ->
    let new_result =
      match Environment.lookup env v with
      | Compound ("block", terms) ->
        (* Var is a block, so append the term with hole and continue *)
        let matches = Compound ("block", terms @ [term]) in
        Format.printf "3. Adding var %s with %s@." s (Term.to_string matches);
        (Environment.add env v matches), loc (* XXX *)
      | Var _ ->
        (* Var does not exist, so add a term and continue *)
        Format.printf "4. Adding var %s with %s@." s (Term.to_string term);
        (Environment.add env v term), loc (* XXX *)
      (* var has only been matched with one term, extend it to be a
         compound. and continue *)
      | existing_term ->
        Format.printf "Var is currently bound to %s@." (Term.to_string existing_term);
        let matches = Compound ("block", [existing_term; term]) in
        Format.printf "5. Adding var %s with %s@." s (Term.to_string matches);
        (Environment.add env v matches), loc  (* XXX *)
    in
    new_result

  | [Var ((s,_) as v)], last_terms ->
    let new_result =
      match Environment.lookup env v with
      | Compound ("block", existing_terms) ->
        (* Var is a block, so append the term with hole and stop *)
        let matches = Compound ("block", existing_terms @ last_terms) in
        Format.printf "3. Adding var %s with %s@." s (Term.to_string matches);
        (Environment.add env v matches), loc (* XXX *)
      | Var _ ->
        (* Var does not exist, so add a block term and stop *)
        let term = Compound ("block", last_terms) in
        Format.printf "4. Adding var %s with %s@." s (Term.to_string term);
        (Environment.add env v term), loc (* XXX *)
      (* var has only been matched with one term, extend it to be a
         compound. and continue *)
      | existing_term ->
        Format.printf "Var is currently bound to %s@." (Term.to_string existing_term);
        let matches = Compound ("block", [existing_term] @ last_terms) in
        Format.printf "5. Adding var %s with %s@." s (Term.to_string matches);
        (Environment.add env v matches), loc  (* XXX *)
    in
    new_result

  | Const c1::rest1, Const c2::rest2 when c1 = c2 ->
    find_list env rest1 rest2

  | Compound ("block", ts1)::rest1, Compound ("block", ts2)::rest2 ->
    let env,_ = find_list env ts1 ts2 in (* XXX what about loc *)
    find_list env rest1 rest2

  | Compound (c1, [b1])::rest1, Compound (c2, [b2])::rest2 ->
    let env,_ = find_aux env b1 b2 in
    find_list env rest1 rest2

  | Compound (c1, [])::rest1, Compound (c2, [])::rest2 ->
    find_list env rest1 rest2

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
