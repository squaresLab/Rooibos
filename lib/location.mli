type t =
  { line    : int
  ; column  : int
  ; offset  : int
  }

(* [make] constructs a location from a lexer position [pos] *)
val make : Lexing.position -> t

module Range : sig
  type k = t
  type t =
    { start : k
    ; stop : k
    }

  (* [start range] returns the location at which [range] begins. *)
  val start : t -> k

  (* [stop range] returns the location at which [range] ends. *)
  val stop : t -> k

  (* [make start stop] constructs a location range that spans all characters
      starting from [start] until [stop], inclusive. *)
  val make : k -> k -> t

  val to_string : t -> string

  val mock : t
end

val to_string : t -> string
