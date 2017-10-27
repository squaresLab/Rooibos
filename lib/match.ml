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

(**
N_debug(C(x), C(=), H(1), C(;))
N_debug(C(x), C(=), C(foo), C(;), C(x), C(=), C(bar), C(;))
*)

let rec find_aux env template source : (Environment.t * Location.Range.t) =
  match template, source with
  | Const c1, Const c2 when c1 = c2 -> env, loc
  | Break, Break -> env, loc
  | Compound ("block", ts1), Compound ("block", ts2) ->
    let r = find_list env ts1 ts2 in
    Format.printf "XXX@.";
    r
  | Compound (c1, [b1]), Compound(c2, [b2]) when c1 = c2 ->
    let env, _ = find_aux env b1 b2 in
    env, loc
  | Var v, term ->
    (Environment.add env v term), loc
  | _, _ -> raise NoMatch
and find_list env ts1 ts2 =
  Format.printf "Matching %s@.\
                 With     %s@.@."
    (Term.to_string (Compound ("debug", ts1)))
    (Term.to_string (Compound ("debug", ts2)));

  match ts1, ts2 with
  | Const c1::((Var ((s,_) as v) :: rest1) as continue),
    Const c2::start::rest2
    when c1 = c2 ->
    Format.printf "Adding var %s with term %s@." s (Term.to_string start);
    let env = Environment.add env v (Compound ("block", [start])) in
    find_list env continue rest2

  | Var v::((suffix::rest) as continue),
    ((term::rest2) as continue2) ->
    if suffix = term
    then find_list env continue continue2
    else begin match Environment.lookup env v with
      | Compound ("block", terms) ->
        let matches = Compound ("block", terms @ [term]) in
        Environment.add env v matches, loc
      | _ -> failwith "This hole should only ever be compound when matching lists"
    end

  | [Var v], [term] ->
    Environment.add env v term, loc

  | [Var v], terms ->
    Environment.add env v (Compound ("block", terms)), loc

  | term1::rest1,
    term2::rest2
    when term1 = term2 ->
    find_list env rest1 rest2

  | [], _ ->
    env, loc

  | _, _ -> raise NoMatch


let find template source =
  let env = Environment.create () in
  try Some (find_aux env template source)
  with _ -> None

let exists template source = false

let to_string match_ = ""
