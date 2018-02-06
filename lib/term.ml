open Core

type constant = string

type variable = string * int

type t =
  | Break of Location.Range.t
  | White of constant * Location.Range.t
  | Var of variable * Location.Range.t
  | Const of constant * Location.Range.t
  | Compound of constant * t list * Location.Range.t


let rec contains term variable =
  match term with
  | Var (y, _) -> variable = y
  | Compound (_, ts, _) ->
    List.exists ~f:(fun term -> contains term variable) ts
  | Const _
  | White _
  | Break _ -> false

let range = function
  | Break loc -> loc
  | White (_, loc) -> loc
  | Var (_, loc) -> loc
  | Const (_, loc) -> loc
  | Compound (_, _, loc) -> loc

let rec strip term =
  let l = Location.Range.unknown in
  match term with
  | Break _ -> Break l
  | White (w, _) -> White (w, l)
  | Var (v, _) -> Var (v, l)
  | Const (c, _) -> Const (c, l)
  | Compound (c, ls, _) ->
    let ls = List.map ~f:strip ls in
      Compound (c, ls, l)

let rec _to_string (with_location : bool) (term : t) =
  let loc =
  match with_location with
  | true -> "[" ^ (Location.Range.to_string (range term)) ^ "]"
  | false -> ""
  in
  match term with
  | Break _ -> "CR" ^ loc
  | White (w, _) -> "W" ^ loc ^ (Format.sprintf "(%S)" w)
  | Var ((v, 0), _) -> "H" ^ loc ^ "(" ^ v ^ ")"
  | Var ((v, n), _) ->
    "H" ^ loc ^ "(" ^ v ^ ", " ^ (string_of_int n) ^ ")"
  | Const (c, _) -> "C" ^ loc ^ "(" ^ c ^ ")"
  | Compound (f, ls, _) ->
    let (prefix, suffix) = begin match f with
      | "block" -> (loc ^ "<"), ">"
      | _ -> ("N_" ^ f ^ loc ^ "("), ")"
    end in
    prefix ^ (String.concat ~sep:", " (List.map ~f:(_to_string with_location) ls)) ^ suffix

and to_string_with_loc term = _to_string true term
and to_string term = _to_string false term
