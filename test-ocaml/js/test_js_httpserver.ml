open OUnit

module StringMap = HttpServer_js.StringMap

let buffer_of_string = HttpServer_js.buffer_of_string

let test_httpserver_get () =
  let server = HttpServer_js.create_server (fun server req send_res ->
    assert_equal ~printer:(fun x->x) req.HttpServer_js.hr_url "/path";
    assert_equal ~printer:(fun x->x) req.HttpServer_js.hr_body "";
    assert_equal ~printer:(fun x->x) req.HttpServer_js.hr_method "GET";
    assert_equal ~printer:(fun x->x) (StringMap.find "testheader" req.HttpServer_js.hr_headers) "Test";
    send_res 200 "text/plain" (buffer_of_string "hello from server"))
  in
  Js.Unsafe.global##__stopServer1 <- Js.wrap_callback (fun () -> HttpServer_js.stop_server server);
  HttpServer_js.start_server server ~port:8080 ~host:"127.0.0.1" ~err_callback:(fun _ -> ()) ~callback:(fun () ->
    Js.Unsafe.eval_string "
      var http = require('http');

      var options = {
        hostname : 'localhost',
        port : 8080,
        path : '/path',
        method : 'GET',
        headers : {
          TestHeader : 'Test',
        },
      };

      var req = http.request(options, function(res) {
        if (res.statusCode != 200) {
          throw new Error('status code is not 200');
        }

        var body = '';

        res.on('data',function(chunk) {
          body += chunk.toString();
        });

        res.on('end',function() {
          if (body != 'hello from server') {
            throw new Error('body is not same as expected.');
          }
          _stopServer1();
        });
      });

      req.end();
    " |> ignore)

let test_httpserver_post () =
  let server = HttpServer_js.create_server (fun server req send_res ->
    assert_equal ~printer:(fun x->x) req.HttpServer_js.hr_url "/path";
    assert_equal ~printer:(fun x->x) req.HttpServer_js.hr_body "hello world";
    assert_equal ~printer:(fun x->x) req.HttpServer_js.hr_method "POST";
    assert_equal ~printer:(fun x->x) (StringMap.find "testheader" req.HttpServer_js.hr_headers) "Test";
    send_res 200 "text/plain" (buffer_of_string "hello from server"))
  in
  Js.Unsafe.global##__stopServer2 <- Js.wrap_callback (fun () -> HttpServer_js.stop_server server);
  HttpServer_js.start_server server ~port:8081 ~host:"127.0.0.1" ~err_callback:(fun _ -> ()) ~callback:(fun () ->
    Js.Unsafe.eval_string "
      var http = require('http');

      var options = {
        hostname : 'localhost',
        port : 8081,
        path : '/path',
        method : 'POST',
        headers : {
          TestHeader : 'Test',
        },
      };

      var req = http.request(options, function(res) {
        if (res.statusCode != 200) {
          throw new Error('status code is not 200');
        }

        var body = '';

        res.on('data',function(chunk) {
          body += chunk.toString();
        });

        res.on('end',function() {
          if (body != 'hello from server') {
            throw new Error('body is not same as expected.');
          }
          _stopServer2();
        });
      });

      req.write('hello world');
      req.end();
    " |> ignore)


let tests = "httpserver" >::: [
  TestCase test_httpserver_get;
  TestCase test_httpserver_post;
]

