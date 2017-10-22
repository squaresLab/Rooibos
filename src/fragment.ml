(* I don't like the name of this type, but meh! *)
type bracket_t =
  [ `Square
  | `Curly
  | `Angle
  | `Round ]

type t =
  [ `Nested of bracket_t * t
  | `Hole of string
  | `Code of string
  | `Block of t list ]

let rec print fragment =
  match fragment with
  | `Nested(_, f) -> Format.sprintf "Nested(%s)" (print f)
  | `Hole(name) -> Format.sprintf "Hole(%s)" name
  | `Code(s) -> Format.sprintf "Code(%s)" s
  | `Block(fs) ->
      let fs = String.concat "; " (List.map print fs) in
        Format.sprintf "Block([%s])" fs
