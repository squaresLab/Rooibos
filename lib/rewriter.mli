(* [substitute env tpl] completes a given template [tpl] using bindings
   provided by environment [env]. *)
val substitute : Environment.t -> Term.t -> Term.t

(* [rewrite match src tpl] transforms a snippet of code found by a
   match [match] according to a given rewrite template [tpl]. *)
val rewrite : Match.t -> Term.t -> Term.t -> Term.t
