open Core

type constant = string

type variable = string * int

type t =
  | Break     of Location.Range.t
  | White     of constant * Location.Range.t
  | Var       of variable * Location.Range.t
  | Const     of constant * Location.Range.t
  | Compound  of constant * t list * Location.Range.t


let rec contains term variable =
  match term with
  | Var (y, _) -> variable = y
  | Compound (_, ts, _) ->
    List.exists ~f:(fun term -> contains term variable) ts
  | Const _
  | White _
  | Break _ -> false

let range = function
  | Break     loc         -> loc
  | White     (_, loc)    -> loc
  | Var       (_, loc)    -> loc
  | Const     (_, loc)    -> loc
  | Compound  (_, _, loc) -> loc

let rec to_string = function
  | Break _ -> "CR"
  | White (w, _) -> Format.sprintf "W(%S)" w
  | Var ((v, 0), _) -> "H(" ^ v ^ ")"
  | Var ((v, n), _) -> "H(" ^ v ^ ", " ^ (string_of_int n) ^ ")"
  | Const (c, _) -> "C(" ^ c ^ ")"
  | Compound (f, ls, _) ->
    let (prefix, suffix) = match f with
      | "block" -> "<", ">"
      | _ -> ("N_" ^ f ^ "("), ")"
    in
    prefix ^ (String.concat ~sep:", " (List.map ~f:to_string ls)) ^ suffix
