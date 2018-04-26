open Core

type t = Location.Range.t * Environment.t

(** [range mtch] returns the location range where [mtch] was matched. *)
val range : t -> Location.Range.t

(** [environment mtch] returns the environment associated with [mtch]. *)
val environment : t -> Environment.t

(** [all template src] finds all matches of a template [template] in a given
   source code [src]. *)
val all : Term.t -> Term.t -> t Sequence.t

(** [find template src] finds the first match of a template [template] in a given
   source code [src]. *)
val find : Term.t -> Term.t -> t option

(** [exists template src] returns [true] if a given template [template] matches a
   source code [src] in at least one location. *)
val exists : Term.t -> Term.t -> bool
