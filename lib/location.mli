type t

module Range : sig
  type k = t
  type nonrec t =
    { start_ : t
    ; end_ : t
    }

  (* [make start_ stop_] constructs a location range that spans all characters
      starting from [start] till [stop], inclusive. *)
  val make : k -> k -> t

  (* [start range] returns the location at which [range] begins. *)
  val start : t -> k

  (* [stop range] returns the location at which [range] ends. *)
  val stop : t -> k

  val to_string : t -> string
end

(* [make] constructs a location from a lexer position [pos] *)
val make : Lexing.position -> t

(* [offset loc] returns the character offset for location [loc] *)
val offset : t -> int

(* [line_no loc] returns the number of the line at which location
   [loc] occurs. *)
val line_no : t -> int

(* [file_name loc] returns the name of the file at which location
   [loc] occurs. *)
val file_name : t -> string

val to_string : t -> string
