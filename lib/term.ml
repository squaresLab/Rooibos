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
