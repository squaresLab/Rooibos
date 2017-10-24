open Core_kernel

type constant = string

type variable = string * int

type t =
  | Var of variable
  | Const of constant
  | Compound of constant * t list


let rec contains term variable =
  match term with
    Var y -> variable = y
  | Const _ -> false
  | Compound (_, ts) -> List.exists ~f:(fun term -> contains term variable) ts


let rec to_string = function
  | Var (v, 0) -> v
  | Var (v, n) -> v ^ string_of_int n
  | Const c -> c
  | Compound (f, ls) -> f ^ "(" ^ (String.concat ~sep:", " (List.map ~f:to_string ls)) ^ ")"
