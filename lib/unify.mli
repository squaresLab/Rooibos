(** [NoUnify] is raised when terms cannot be unified. *)
exception NoUnify


(** [unify_terms env t1 t2] attempts to unify terms [t1] and [t2]
    in the context of environment [env]. On success, it returns the
    environment extended with the result of unification. On failure,
    it raises [NoUnify]. *)
val unify_terms : Environment.t -> Term.t -> Term.t -> Environment.t
