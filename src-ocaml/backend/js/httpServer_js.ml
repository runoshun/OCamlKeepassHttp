open Js

class type js_httpserver = object end

class type js_request = object
  method _method     : js_string t prop
  method _url        : js_string t prop
  method _body       : js_string t prop
  method _headerKeys : js_string t js_array t prop
  method _headerVals : js_string t js_array t prop
end

class type js_libhttpserver = object
  method createServer : (js_httpserver t -> js_request t -> (js_string t -> js_string t -> unit) -> unit) callback
                        -> js_httpserver t meth
  method startServer  : js_httpserver t -> number t -> js_string t -> (unit -> unit) callback -> unit meth
  method stopServer   : js_httpserver t -> unit meth
end

let libhttpserver : js_libhttpserver t = Js.Unsafe.eval_string "
  (function() {
    var http = require('http');

    return {
        createServer : function(handler) {
            var server = http.createServer(function(req,res) {
              var body = '';
              req.on('data', function(chunk) {
                  body += chunk.toString();
                });
              req.on('end', function() {
                  var headerKeys = Object.keys(req.headers);
                  var headerVals = headerKeys.map(function(k) { return req.headers[k]; });
                  var request = {
                      method     : req.method,
                      url        : req.url,
                      headerKeys : headerKeys,
                      headerVals : headerVals,
                      body       : body,
                  };
                  handler(server, request, function(content_type,response) {
                      res.writeHead(200, {
                          'Content-Length' : response.length,
                          'Content-Type' : content_type
                      });
                      res.end(response);
                  });
              });
            });
            return server;
        },

        startServer : function(server, port, host, callback) {
            server.listen(port,host,511,callback());
        },
        stopServer : function(server) {
            server.close();
        }
    };
  })();"

type httpserver = js_httpserver t

module StringMap = Map.Make(String)

type raw_request = {
  hr_method  : string;
  hr_body    : string;
  hr_url     : string;
  hr_headers : string StringMap.t;
}

let create_server handler =
  let js_handler = Js.wrap_callback (fun server (js_req:js_request t) send_res ->
    let header_keys = Array.to_list (Js.to_array (js_req##_headerKeys)) in
    let header_vals = Array.to_list (Js.to_array (js_req##_headerVals)) in
    let headers = List.fold_left2
      (fun m k v -> StringMap.add (String.lowercase (Js.to_string k)) (Js.to_string v) m)
      StringMap.empty header_keys header_vals in
    let req = {
      hr_method = Js.to_string (js_req##_method);
      hr_body   = Js.to_string (js_req##_body);
      hr_url    = Js.to_string (js_req##_url);
      hr_headers = headers } in
    handler server req (fun content_type res -> send_res (Js.string content_type) (Js.string res)))
  in
  libhttpserver##createServer (js_handler)

let start_server server ~port ~host ~callback =
  let js_port = Js.number_of_float (float_of_int port) in
  libhttpserver##startServer (server,js_port,Js.string host,Js.wrap_callback callback)

let stop_server server =
  libhttpserver##stopServer (server)

