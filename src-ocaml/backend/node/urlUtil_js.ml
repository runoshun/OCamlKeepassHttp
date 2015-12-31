open Js

class type js_parsed_url = object
  method hostname : js_string t opt prop
end

class type js_url = object
  method parse : js_string t -> js_parsed_url t meth
end

let js_url : js_url t = NodeUtils.require "url"

let get_host_name url =
  let parsed = js_url##parse (Js.string url) in
  Opt.case (parsed##.hostname)
    (fun () -> None)
    (fun s  -> Some (Js.to_string s))

