open Core

type constant = string

type variable = string * int

type t =
  | Break
  | Var of variable
  | Const of constant
  | Compound of constant * t list


let rec contains term variable =
  match term with
    Var y -> variable = y
  | Const _ | Break -> false
  | Compound (_, ts) -> List.exists ~f:(fun term -> contains term variable) ts

let range _ =
  let open Location in
  { Range.start_ = { line = 0; column = 0 }
  ; Range.end_ = { line = 0; column = 0 }
  }

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
       prefix ^ (String.concat ~sep:", " (List.map ~f:to_string ls)) ^ suffix
