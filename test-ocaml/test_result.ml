open OUnit

let from_none () =
  let res = MyResult.from_option None "is none" in
  match res with
  | MyResult.Ok _ -> assert_failure "from_option is failed. Ok returned."
  | MyResult.Error s -> assert_equal ~printer:(fun x -> x) s "is none"

let from_some () =
  let res = MyResult.from_option (Some 1) "is none" in
  match res with
  | MyResult.Ok i -> assert_equal i 1
  | MyResult.Error _ -> assert_failure "from_option is failed. Error returned."

let bind_ok () =
  let open MyResult.Monad in
  (MyResult.Ok 1) >>= (fun x ->
    assert_equal ~printer:string_of_int x 1;
    return (MyResult.Ok x)) |> ignore

let bind_error () =
  let open MyResult.Monad in
  (MyResult.Error "error") >>= (fun x ->
    assert_failure "bind_error is failure" |> ignore;
    return (MyResult.Ok 1)) |> ignore

let bind_mix () =
  let open MyResult.Monad in
  let err x = MyResult.Error "error" in
  ((MyResult.Ok 1) >>= fun x ->
  err x >>= fun y ->
  assert_failure "bind_error is failure" |> ignore;
  return (MyResult.Ok 1))
  |> ignore

let tests = "result" >::: [
  TestCase from_none;
  TestCase from_some;
  TestCase bind_ok;
  TestCase bind_error;
  TestCase bind_mix;
]
