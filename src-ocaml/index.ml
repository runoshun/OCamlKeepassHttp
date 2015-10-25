
module App = App.Make(Backends.Js)

let start () =
  Logger.init (module Backends.Js);
  App.main (Js.to_string (Js.Unsafe.variable "__dirname"))

let () =
  let m = Js.Unsafe.js_expr "module" in
  m##exports <- Js.Unsafe.obj [|
    ("start", Js.Unsafe.inject (Js.wrap_callback start))
  |]

