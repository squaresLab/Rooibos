type t =
  { line    : int
  ; column  : int
  ; offset  : int
  }

(* [make] constructs a location from a lexer position [pos] *)
val make : Lexing.position -> t

module Range : sig
  type nonrec t =
    { start : t
    ; stop : t
    }

  (* [make start stop] constructs a location range that spans all characters
      starting from [start] until [stop], inclusive. *)
  val make : k -> k -> t

  val to_string : t -> string
end

val to_string : t -> string
