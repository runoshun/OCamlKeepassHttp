
module App = App.Make(Backends.Js)

let start () =
  Logger.init (module Backends.Js);
  App.main ()

let () =
  let m = Js.Unsafe.js_expr "module" in
  m##exports <- Js.Unsafe.obj [|
    ("start", Js.Unsafe.inject (Js.wrap_callback start))
  |]

