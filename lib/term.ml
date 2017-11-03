open Core_kernel

type constant = string

type variable = string * int

(* TODO: add a dedicated variant for sequences *)
type t =
  | Break
  | Var of variable
  | Const of constant
  | Compound of constant * (t Node.t) list


let rec contains term variable =
  match term with
    Var y -> variable = y
  | Const _ | Break -> false
  | Compound (_, ts) ->
      List.exists ~f:(fun node -> contains (Node.term node) variable) ts

let rec to_string = function
  | Break -> "CR"
  | Var (v, 0) -> "H(" ^ v ^ ")"
  | Var (v, n) -> "H(" ^ v ^ ", " ^ (string_of_int n) ^ ")"
  | Const c -> "C(" ^ c ^ ")"
  | Compound (f, ls) ->
    let (prefix, suffix) = match f with
    | "block" -> "<", ">"
    | _ -> ("N_" ^ f ^ "("), ")"
    in
    let ls = List.map ~f:Node.term ls in
    prefix ^ (String.concat ~sep:", " (List.map ~f:to_string ls)) ^ suffix

let rec compare term1 term2 =
  match term1, term2 with
  | Break, Break -> true
  | Const c1, Const c2 when c1 = c2 -> true
  | Var v1, Var v2 when v1 = v2 -> true
  | Compound (c1,l1), Compound (c2,l2) when c1 = c2 ->
    compare_list l1 l2
  | _ -> false

and compare_list l1 l2 =
  match l1,l2 with
  | [],[] -> true
  | {term = hd1}::tl1, {term = hd2}::tl2
    when (compare hd1 hd2) ->
    compare_list tl1 tl2
  | _ -> false
