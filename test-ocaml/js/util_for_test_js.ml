
module NodeJsBackend : Util_for_test.Interface = struct

  let callback_name_counter = ref 0
  let callback_name () =
    callback_name_counter := !callback_name_counter + 1;
    "nodejstestutilcallback" ^ (string_of_int !callback_name_counter)

  let http_request ~host ~port ~meth ~body ~callback =
    let name = callback_name () in
    let js_callback = Js.wrap_callback (fun body -> (callback (Js.to_string body))) in
    Js.Unsafe.set Js.Unsafe.global (Js.string name) js_callback;
    let code = Printf.sprintf "
      var http = require('http');

      var options = {
        hostname : '%s',
        port : %d,
        method : '%s',
      };

      var req = http.request(options, function(res) {
        var body = '';

        res.on('data',function(chunk) {
          body += chunk.toString();
        });

        res.on('end',function() {
          %s(body);
        });
      });

      req.write('%s');
      req.end();
    " host port meth name body
    in
    Js.Unsafe.eval_string code

end
