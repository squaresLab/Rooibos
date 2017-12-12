type t =
  { line    : int
  ; column  : int
  ; offset  : int
  }

module Range : sig
  type nonrec t =
    { start : t
    ; stop : t
    }

  val to_string : t -> string
end

val to_string : t -> string
