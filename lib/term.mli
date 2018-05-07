type constant = string

type variable = string * int

(** Terms are used to represent holes in the code (i.e., variables),
    concrete snippets of code (i.e., constants), and
    (optionally) bracket-delimited sequences of terms
    (i.e., compound terms). *)
type t =
  | Break of Location.Range.t
  | White of constant * Location.Range.t
  | Comment of constant * Location.Range.t
  | Var of variable * Location.Range.t
  | Const of constant * Location.Range.t
  | Compound of constant * t list * Location.Range.t

(** [equivalent x y] checks whether term [x] is equivalent to term [y].
 *  I.e., the content of [x] is compared against the content of [y] for
 *  equality; location data is ignored. *)
val equivalent : t -> t -> bool

(** [contains term var] returns [true] when term [term] contains
    variable instance [var]. *)
val contains : t -> variable -> bool

(** [range term] returns the range of locations covered by term [term]. *)
val range : t -> Location.Range.t

(** [strip term] returns a given term [term] with its location information
 *  replaced with mock data. *)
val strip : t -> t

val to_string : t -> string

val to_string_with_loc : t -> string
