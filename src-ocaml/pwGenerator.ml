
let chars_lower = "abcdefghijklmnopqrstuvwxyz"
let chars_upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let chars_numbers = "0123456789"
let chars_all = chars_lower ^ chars_upper ^ chars_numbers

let gen available_chars length =
  let len_chars = String.length available_chars in
  let indexies = Array.init length (fun _ -> Random.int len_chars) in
  let buf = Buffer.create length in
  Array.iter (fun v -> Buffer.add_char buf available_chars.[v]) indexies;
  Buffer.contents buf
