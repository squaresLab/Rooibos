type constant = string

type variable = string * int

type t =
  | Var of variable
  | Const of constant
  | Compound of constant * t list

(** [contains t x] returns [true] when term [t] contains variable instance [x]  *)
val contains : t -> variable -> bool

val to_string : t -> string
