type t

val pp : Format.formatter -> t -> unit
val show : t -> string

val encode : string -> t
val decode : t -> string

val of_string : string -> t
val to_string : t -> string
