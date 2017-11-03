(** Snippet type? *)
open Core_kernel

type t = Environment.t * Location.Range.t

(* [all tpl src] finds all matches of a template [tpl] in a given source
   code [src]. *)
val all : Term.t Node.t -> Term.t Node.t -> t Sequence.t

(* [find tpl src] finds the first match of a template [tpl] in a given
   source code [src]. *)
val find : Term.t Node.t -> Term.t Node.t -> t option

(* [exists tpl src] returns [true] if a given template [tpl] matches a source
   code [src] in at least one location. *)
val exists : Term.t Node.t -> Term.t Node.t -> bool

val to_string : t -> string
