open Term

(** Environments provide a mapping from template "variables" (i.e., holes)
    to "terms" (i.e., snippets of code) in a concrete source code
    instance. *)
type t


val create : unit -> t

(** [add env var term] binds the term [term] to variable [var] in
    environment [env]. *)
val add : t -> variable -> Term.t -> t

(** [lookup env var] returns the value of variable instance [var] in
    environment [env]. Returns [Var var] if [var] does not occur
    in [env]. *)
val lookup : t -> variable -> Term.t

(** [substitute env term] substitutes in term [term] values for
    variables, as specified by environment [env]. It substitutes
    repeatedly until the terms stops changing, so this is not the
    usual kind of substitution. It is what we need during
    unification. *)
val substitute : t -> Term.t -> Term.t

val to_string : t -> string
