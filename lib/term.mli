type constant = string

type variable = string * int

(** Terms are used to represent holes in the code (i.e., variables),
    concrete snippets of code (i.e., constants), and
    (optionally) bracket-delimited sequences of terms
    (i.e., compound terms).*)
type t =
  | Break
  | Var of variable
  | Const of constant
  | Compound of constant * t list

(** [contains term var] returns [true] when term [term] contains
    variable instance [var] *)
val contains : t -> variable -> bool

(** [range term] returns the range of locations covered by term [term]. *)
val range : t -> Location.Range.t

val to_string : t -> string
