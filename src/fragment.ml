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

let rec to_s (fragment : t) : string =
  match fragment with
  | `Nested(_, f) -> Format.sprintf "Nested(%s)" (to_s f)
  | `Hole(name) -> Format.sprintf "Hole(%s)" name
  | `Code(s) -> Format.sprintf "Code(%s)" s
  | `Block(fs) ->
      let fs = String.concat "; " (List.map to_s fs) in
        Format.sprintf "Block([%s])" fs

let print (fragment : t) : unit =
  print_string (to_s fragment)
