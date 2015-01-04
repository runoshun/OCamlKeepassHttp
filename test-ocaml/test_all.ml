open OUnit

let () =
  Logger.init (module Backends.Js);
  Logger.set_log_level 0;
  Test_js_keepassdb.init (fun () ->
    Test_keepassProvider.init (fun () ->
      Test_keepassHttpServer.init (fun () ->
      run_test_tt_main ("all" >::: [
        Test_uuid.tests;
        Test_result.tests;
        Test_js_cipher.tests;
        Test_js_urlutils.tests;
        Test_js_keepassdb.tests;
        Test_js_httpserver.tests;
        Test_keepassProvider.tests;
        Test_keepassHttpServer.tests;
      ]) |> ignore)))
