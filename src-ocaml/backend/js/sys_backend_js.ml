open Js

class type js_env = object end

class type js_stdout = object
  method write : js_string t -> unit meth
end

class type js_process = object
  method stdout : js_stdout t readonly_prop
  method argv : js_string t js_array t readonly_prop
  method env  : js_env t readonly_prop
end

class type js_fs = object
  method readFileSync : js_string t -> js_string t meth
end

let js_process : js_process t = Js.Unsafe.global##process
let js_fs : js_fs t = NodeUtils.require "fs"

let print msg =
  js_process##stdout##write (Js.string msg)

let command_line_args =
  let arr = Js.to_array js_process##argv in
  if Array.length arr < 1 then
    [| |]
  else
    Array.map (fun s -> Js.to_string s) (Array.sub arr 1 (Array.length arr - 1))

let read_file path =
  let buf = (js_fs##readFileSync (Js.string path)) in
  Js.to_string (buf##toString ())

let get_env var =
  Optdef.case (Js.Unsafe.get js_process##env var)
    (fun () -> None)
    (fun e  -> Some (Js.to_string e))

