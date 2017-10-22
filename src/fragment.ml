(* I don't like the name of this type, but meh! *)
type bracket_t =
  | `Square
  | `Curly
  | `Angle
  | `Round

type t =
  | `None
  | `Nested of bracket_t * t
  | `Hole of string
  | `Code of string
  | `Block of t list
