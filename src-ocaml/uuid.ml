
type t = int array

let to_string t =
  Printf.sprintf "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
    t.(0) t.(1) t.(2) t.(3) t.(4) t.(5) t.(6) t.(7)
    t.(8) t.(9) t.(10) t.(11) t.(12) t.(13) t.(14) t.(15)

let of_string s =
  try
    MyResult.Ok
      (Scanf.sscanf s "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
       (fun x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 ->
         [| x0; x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12; x13; x14; x15; |]))
  with
  | Scanf.Scan_failure s ->
      MyResult.Error ("can't parse as uuid: " ^ s)

let to_hex_base64 t =
  let bytes = Bytes.init 16 (fun i -> char_of_int t.(i)) in
  Base64.encode (Bytes.to_string bytes)

let gen_v4 () =
  Random.self_init ();
  (* 1001 00xx xxxx xxxx *)
  let fst_rand () = 0x90 + Random.int (0x3 + 1) in
  let rand_8 () = Random.int (0xFF + 1) in
  Array.init 16 (fun i -> if i = 0 then fst_rand () else rand_8 ())

let gen_v4_hex_base64 () =
  let uuid = gen_v4 () in
  to_hex_base64 uuid
