type 'a t =
  | Ok of 'a
  | Error of string

let from_option opt err =
  match opt with
  | Some x -> Ok x
  | None   -> Error err

let case t if_ok if_err =
  match t with
  | Ok x -> if_ok x
  | Error msg -> if_err msg

module Monad = struct
  let bind m f =
    match m with
    | Ok x -> f x
    | Error x -> Error x

  let return x = Ok x
  let fail x = Error x

  let (>>=) = bind
end
