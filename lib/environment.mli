open Term

(** Environments provide a mapping from template "variables" (i.e., holes)
    to nodes (i.e., snippets of code) in a concrete source code
    instance. *)
type t


val create : unit -> t

(** [add env var node] binds the node [node] to variable [var] in
    environment [env]. *)
val add : t -> variable -> Term.t Node.t -> t

(** [lookup env var] fetches the node bound to a given variable [var] in
    environment [env]. *)
val lookup : t -> variable -> Term.t Node.t option

(** [substitute env node] performs substitution on a given node [node],
    according to the bindings specified by environment [env]. *)
val substitute : t -> Term.t Node.t -> Term.t Node.t

val to_string : t -> string
