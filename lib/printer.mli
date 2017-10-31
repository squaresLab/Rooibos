open Core_kernel
open Format

val to_string : Term.t -> string

val pp : formatter -> Term.t -> unit

val pps : unit -> Term.t -> string

val ppo : Out_channel.t -> Term.t -> unit
