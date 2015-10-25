open OUnit

module KeepassProviderTests(Backends : Backends.Interface) = struct

  module Provider = KeepassProvider.Make(Backends)
  module KeepassDb = Backends.KeepassDb

  let config = Provider.make_config "test-data/provider.kdbx" ~password:(Some "test") ~keyfile:None
  let provider_ref = ref (Result.Error "provider is not set.")

  let init thunk =
    Provider.with_provider config (fun provider_or_error ->
      match provider_or_error with
      | Result.Ok provider ->
          provider_ref := (Result.Ok provider);
          thunk ()
      | Result.Error e ->
          provider_ref := (Result.Error e))

  let test_client_config () =
    match !provider_ref with
    | Result.Error m -> assert_failure m
    | Result.Ok provider ->
        let key = (Base64.encode "key") in
        Provider.create_client_config provider
          ~client_id:"client 1"
          ~client_key:key;
        let client_key = Provider.get_client_key provider "client 1" in
        let no_client_key = Provider.get_client_key provider "invalid client" in
        assert_equal (Result.Ok key) client_key;
        match no_client_key with
        | Result.Ok _ -> assert_failure "expected not found, but client_key found."
        | Result.Error _ -> ()

  let test_client_login () =
    match !provider_ref with
    | Result.Error m -> assert_failure m
    | Result.Ok provider ->
        Provider.create_login
          provider
          ~url:"http://www.provider.example.com/provider_test"
          ~login:"login"
          ~password:"password";
        let printer = function Some s -> s | None -> "None" in
        let entries = Provider.search_entries provider (fun entry ->
          entry.KeepassDb.kp_title = (Some "www.provider.example.com")) in
        assert_equal ~printer:string_of_int 1 (List.length entries);
        List.iter (fun entry ->
          assert_equal ~printer (Some "http://www.provider.example.com/provider_test") entry.KeepassDb.kp_url;
          assert_equal ~printer (Some "login") entry.KeepassDb.kp_username;
          assert_equal ~printer (Some "password") entry.KeepassDb.kp_password;
          assert_bool "uuid is empty." (entry.KeepassDb.kp_uuid != ""))
          entries

  let tests = "KeepassProviderTests" >::: [
    TestCase test_client_config;
    TestCase test_client_login;
  ];

end

module NodeJsBackendTest = KeepassProviderTests(Backends.Js)

let init thunk =
  NodeJsBackendTest.init (fun () ->
    thunk ())

let tests = "KeepassProvider" >::: [
  NodeJsBackendTest.tests;
]

