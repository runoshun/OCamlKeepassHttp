
module NodeApp = App.Make(Backends.Js)

let () =
  Logger.init (module Backends.Js);
  NodeApp.main ()
