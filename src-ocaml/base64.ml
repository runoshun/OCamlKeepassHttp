type t = string
[@@deriving show]

let decode x = B64.decode x
let encode x = B64.encode x

let of_string x = x
let to_string x = x
