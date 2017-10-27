(** Snippet type? *)
open Core_kernel

type t = Environment.t * Term.t * Location.t

(* [all tpl src] finds all matches of a template [tpl] in a given source
   code [src]. *)
val all : Term.t -> Term.t -> t Sequence.t

(* [find tpl src] finds the first match of a template [tpl] in a given
   source code [src]. *)
val find : Term.t -> Term.t -> t option

(* [exists tpl src] returns [true] if a given template [tpl] matches a source
   code [src] in at least one location. *)
val exists : Term.t -> Term.t -> bool

val to_string : t -> string
