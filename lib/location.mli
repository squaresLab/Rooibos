type t

module Range : sig
  type k = t
  type t = k * k

  val to_string : t -> string
end

val to_string : t -> string
