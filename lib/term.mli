type constant = string

type variable = string * int

type t =
  | Var of variable
  | Const of constant
  | Compound of constant * t list

(** [contains term var] returns [true] when term [term] contains
    variable instance [var] *)
val contains : t -> variable -> bool

val to_string : t -> string
