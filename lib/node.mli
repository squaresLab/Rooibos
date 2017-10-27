type 'a t = { term : 'a; loc : Location.Range.t }

(* [make term loc] returns a node representing a term [term] that occurs at
   a given location range [loc]. *)
val make : 'a -> 'a t

(* [range node] returns the range of locations covered by node [node]. *)
val range : 'a t -> Location.Range.t

(* [term node] returns the term associated with node [node]. *)
val term : 'a t -> 'a
