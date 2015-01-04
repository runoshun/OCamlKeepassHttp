
let gen_v4 () =
  Random.self_init ();
  (* 1001 00xx xxxx xxxx *)
  let first_rand () = 0x9000 + Random.int (0x3FF + 1) in
  let rand_16 () = Random.int (0xFFFF + 1) in
  let rnd = Array.init 8 (fun i -> if i = 0 then first_rand () else rand_16 ()) in
  Printf.sprintf "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
    rnd.(0) rnd.(1) rnd.(2) rnd.(3)
    rnd.(4) rnd.(5) rnd.(6) rnd.(7)

