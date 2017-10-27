type 'a t = { term : 'a; loc : Location.t }

(* [range node] returns the range of locations covered by node [node]. *)
val range : 'a t -> Location.Range.t

(* [term node] returns the term associated with node [node]. *)
val term : 'a t -> 'a
