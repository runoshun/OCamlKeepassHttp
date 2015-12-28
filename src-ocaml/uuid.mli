
type t

val gen_v4 : unit -> t
val gen_v4_hex_base64 : unit -> Base64.t
val to_string : t -> string
val to_hex_base64 : t -> Base64.t
val of_string : string -> t Result.t
