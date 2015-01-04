open OUnit

module KeepassHttpServerTests
    (Backends : Backends.Interface)
    (TestUtils : Util_for_test.Interface)
    (TestName : sig val name : string end) = struct

  module KeepassHttpServer = KeepassHttpServer.Make(Backends)

  open KeepassHttpServer

  let port_counter = ref 8090
  let get_port () = port_counter := !port_counter + 1; !port_counter

  let make_assert_request req_body expected_req =
    let req_ref = ref None in
    let res_ref = ref "" in
    let server = KeepassHttpServer.create_server (fun server req send_res ->
      req_ref := Some req;
      send_res KeepassHttpServer.ResFailed) in
    let port = get_port () in
    let body = Yojson.Safe.to_string req_body in
    let init thunk = KeepassHttpServer.start_server server ~port:(Some port) ~callback:(fun () ->
      TestUtils.http_request ~host:"localhost" ~port ~meth:"POST" ~body ~callback:(fun body ->
        res_ref := body;
        thunk ())) in
    let test () =
      KeepassHttpServer.stop_server server;
      assert_equal (Some expected_req) !req_ref ~printer:(function
        | Some r -> KeepassHttpServer.show_request r
        | None   -> "None");
      assert_bool "response body is empty" (!res_ref != "")
    in
    (init, test)

  let make_assert_response res expected_res =
    let res_ref = ref `Null in
    let server = KeepassHttpServer.create_server (fun server req send_res ->
      send_res res) in
    let port = get_port () in
    let init thunk = KeepassHttpServer.start_server server ~port:(Some port) ~callback:(fun () ->
      TestUtils.http_request ~host:"localhost" ~port ~meth:"POST" ~body:"{}" ~callback:(fun body ->
        res_ref := Yojson.Safe.from_string body;
        thunk ())) in
    let test () =
      KeepassHttpServer.stop_server server;
      begin match (!res_ref,expected_res) with
      | (`Assoc actual, `Assoc expected) ->
          List.iter (fun (k,v) ->
            assert_equal (List.assoc k expected) v
              ~printer:(function `String s -> s | _ -> "Other"))
            actual
      | _ -> assert_failure "response is not Assoc" |> ignore;
      assert_equal expected_res !res_ref ~printer:Yojson.Safe.to_string;
      end
    in
    (init, test)

  let (init_req_test_associate, test_req_test_associate) =
    let nonce = Backends.Cipher.gen_nonce () in
    make_assert_request
      (`Assoc [
        ("RequestType", `String "test-associate");
        ("Id", `String "client id");
        ("Nonce", `String (Base64.to_string nonce));
        ("Verifier", `String "verifier"); ])
      (ReqTestAssociate
        { req_id = "client id";
          req_nonce = nonce;
          req_verifier = (Base64.of_string "verifier"); })

  let (init_req_associate, test_req_associate) =
    make_assert_request
      (`Assoc [
          ("RequestType", `String "associate");
          ("Key", `String "client key");])
      (ReqAssociate
         { req_key = Base64.of_string "client key"; })

  let (init_req_get_logins, test_req_get_logins) =
    let nonce = Backends.Cipher.gen_nonce () in
    make_assert_request
      (`Assoc [
        ("RequestType", `String "get-logins");
        ("Id", `String "client id");
        ("Nonce", `String (Base64.to_string nonce));
        ("Verifier", `String "verifier");
        ("Url", `String "http://example.com") ])
      (ReqGetLogins (
        { req_id = "client id";
          req_nonce = nonce;
          req_verifier = (Base64.of_string "verifier"); },
        { req_url = (Base64.of_string "http://example.com"); }))

  let (init_req_get_logins_count, test_req_get_logins_count) =
    let nonce = Backends.Cipher.gen_nonce () in
    make_assert_request
      (`Assoc [
        ("RequestType", `String "get-logins-count");
        ("Id", `String "client id");
        ("Nonce", `String (Base64.to_string nonce));
        ("Verifier", `String "verifier");
        ("Url", `String "http://example.com") ])
      (ReqGetLoginsCount (
        { req_id = "client id";
          req_nonce = nonce;
          req_verifier = (Base64.of_string "verifier"); },
        { req_url = (Base64.of_string "http://example.com"); }))

  let (init_req_set_login, test_req_set_login) =
    let nonce = Backends.Cipher.gen_nonce () in
    make_assert_request
      (`Assoc [
        ("RequestType", `String "set-login");
        ("Id", `String "client id");
        ("Nonce", `String (Base64.to_string nonce));
        ("Verifier", `String "verifier");
        ("SubmitUrl", `String "http://example.com");
        ("Login", `String "login");
        ("Password", `String "password"); ])
      (ReqSetLogin (
        { req_id = "client id";
          req_nonce = nonce;
          req_verifier = (Base64.of_string "verifier"); },
        { req_submit_url = Base64.of_string "http://example.com";
          req_login = Base64.of_string "login";
          req_password = Base64.of_string "password"; }))

  let (init_unknown_req, test_unknown_req) =
    make_assert_request
      (`Assoc [
          ("RequestType", `String "unknown")])
      (ReqInvalid)

  let (init_no_req_type, test_no_req_type) =
    make_assert_request
      (`Assoc [
        ("Id", `String "client id");
        ("Verifier", `String "verifier");])
      (ReqInvalid)

  let (init_res_test_associate,test_res_test_associate) =
    let nonce = Backends.Cipher.gen_nonce () in
    make_assert_response
      (ResTestAssociate (
        { res_success = true;
          res_nonce = nonce;
          res_verifier = Base64.of_string "verifier"},
        { res_id = "client id" }))
      (`Assoc [
        ("Success", `Bool true);
        ("Nonce", `String (Base64.to_string nonce));
        ("Verifier", `String "verifier");
        ("Id", `String "client id")])

  let (init_res_associate, test_res_associate) =
    let nonce = Backends.Cipher.gen_nonce () in
    make_assert_response
      (ResAssociate (
        { res_success = true;
          res_nonce = nonce;
          res_verifier = Base64.of_string "verifier"},
        { res_id = "client id" }))
      (`Assoc [
        ("Success", `Bool true);
        ("Nonce", `String (Base64.to_string nonce));
        ("Verifier", `String "verifier");
        ("Id", `String "client id")])

  let (init_res_get_logins, test_res_get_logins) =
    let nonce = Backends.Cipher.gen_nonce () in
    make_assert_response
      (ResGetLogins (
        { res_success = true;
          res_nonce = nonce;
          res_verifier = Base64.of_string "verifier"},
        { res_id = "client id";
          res_entries = [
            { entry_name = Base64.of_string "name1";
              entry_login = Base64.of_string "login1";
              entry_password = Base64.of_string "password1";
              entry_uuid = Base64.of_string "uuid1" } ];
        }))
      (`Assoc [
        ("Success", `Bool true);
        ("Nonce", `String (Base64.to_string nonce));
        ("Verifier", `String "verifier");
        ("Id", `String "client id");
        ("Entries", `List [ `Assoc [
          ("Name", `String "name1");
          ("Login", `String "login1");
          ("Password", `String "password1");
          ("Uuid", `String "uuid1")]])])

  let (init_res_get_logins_count, test_res_get_logins_count) =
    let nonce = Backends.Cipher.gen_nonce () in
    make_assert_response
      (ResGetLoginsCount (
        { res_success = true;
          res_nonce = nonce;
          res_verifier = Base64.of_string "verifier"},
        { res_count = 3 }))
      (`Assoc [
        ("Success", `Bool true);
        ("Nonce", `String (Base64.to_string nonce));
        ("Verifier", `String "verifier");
        ("Count", `Int 3)])

  let (init_res_failed, test_res_failed) =
    make_assert_response
      ResFailed
      (`Assoc [
        ("Success", `Bool false)])

  let tests = TestName.name >::: [
    TestCase test_req_test_associate;
    TestCase test_req_associate;
    TestCase test_req_get_logins;
    TestCase test_req_get_logins_count;
    TestCase test_req_set_login;
    TestCase test_unknown_req;
    TestCase test_no_req_type;

    TestCase test_res_test_associate;
    TestCase test_res_associate;
    TestCase test_res_get_logins;
    TestCase test_res_get_logins_count;
    TestCase test_res_failed;
  ]

  let init thunk =
    let init_thunk = List.fold_left (fun thunk init_fun ->
      (fun () -> init_fun thunk))
    thunk
    [ init_req_test_associate;
      init_req_associate;
      init_req_get_logins;
      init_req_get_logins_count;
      init_req_set_login;
      init_unknown_req;
      init_no_req_type;

      init_res_test_associate;
      init_res_associate;
      init_res_get_logins;
      init_res_get_logins_count;
      init_res_failed;
    ] in
    init_thunk ()

end

module JsKeepassHttpServerTest = KeepassHttpServerTests(Backends.Js)(Util_for_test_js.NodeJsBackend)(struct let name = "JsBackend" end)

let tests = "KeepassHttpServer" >::: [
  JsKeepassHttpServerTest.tests
]

let init thunk =
  JsKeepassHttpServerTest.init thunk
