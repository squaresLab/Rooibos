open Term

type t


val create : unit -> t

(** [add var t] binds the term [t] to variable [var] *)
val add : t -> variable -> Term.t -> t

(** [lookup env x] returns the value of variable instance [x] in
    environment [env]. It returns [Var x] if the variable does not occur
    in [env]. *)
val lookup : t -> variable -> Term.t

(** [substitute env t] substitutes in term [t] values for variables,
    as given in [env]. It substitutes
    repeatedly until the terms stops changing, so this is not the
    usual kind of substitution. It is what we need during
    unification. *)
val substitute : t -> Term.t -> Term.t

val to_string : t -> string
