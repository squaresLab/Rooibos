type t =
  { line : int
  ; column : int
  }

module Range : sig
  type nonrec t =
    { start_ : t
    ; end_ : t
    }

  val to_string : t -> string
end

val to_string : t -> string
