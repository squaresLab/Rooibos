(** [NoUnify] is raised when terms cannot be unified. *)
exception NoUnify


(** [unify_terms env t1 t2] attempts to unify terms [t1] and [t2]
    in the context of environment [env]. On success, it returns the
    environment extended with the result of unification. On failure,
    it raises [NoUnify]. *)
val unify_terms : Environment.t -> Term.t -> Term.t -> Environment.t


(** unify_flat reduces terms t1 and t2 to a flat structure of only hole and
    consts (it is assumed they are well formed) and compounds are filtered out. Then
    it unifies, e.g., (hole * const * hole ...)  with (const * const * ...) *)
val unify_flat : Environment.t -> Term.t -> Term.t -> Environment.t
