open OUnit

let from_none () =
  let res = Result.from_option None "is none" in
  match res with
  | Result.Ok _ -> assert_failure "from_option is failed. Ok returned."
  | Result.Error s -> assert_equal ~printer:(fun x -> x) s "is none"

let from_some () =
  let res = Result.from_option (Some 1) "is none" in
  match res with
  | Result.Ok i -> assert_equal i 1
  | Result.Error _ -> assert_failure "from_option is failed. Error returned."

let bind_ok () =
  let open Result.Monad in
  (Result.Ok 1) >>= (fun x ->
    assert_equal ~printer:string_of_int x 1;
    return (Result.Ok x)) |> ignore

let bind_error () =
  let open Result.Monad in
  (Result.Error "error") >>= (fun x ->
    assert_failure "bind_error is failure" |> ignore;
    return (Result.Ok 1)) |> ignore

let bind_mix () =
  let open Result.Monad in
  let err x = Result.Error "error" in
  ((Result.Ok 1) >>= fun x ->
  err x >>= fun y ->
  assert_failure "bind_error is failure" |> ignore;
  return (Result.Ok 1))
  |> ignore

let tests = "result" >::: [
  TestCase from_none;
  TestCase from_some;
  TestCase bind_ok;
  TestCase bind_error;
  TestCase bind_mix;
]
