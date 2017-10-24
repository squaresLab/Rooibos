open Environment
open Term

exception NoUnify

let rec unify_terms env t1 t2 =
  Format.printf "Unify %s@.\
                 With  %s@." (Term.to_string t1) (Term.to_string t2);
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
    List.fold_left2 (fun env t1 t2 -> unify_terms env t1 t2) env lst1 lst2
  with Invalid_argument _ -> raise NoUnify
