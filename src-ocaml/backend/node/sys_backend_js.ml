open Js

class type js_env = object end

class type js_stdout = object
  method write : js_string t -> unit meth
end

class type js_process = object
  method stdout : js_stdout t readonly_prop
  method chdir  : js_string t -> unit meth
  method argv : js_string t js_array t readonly_prop
  method env  : js_env t readonly_prop
end

class type js_fs = object
  method readFileSync : js_string t -> js_string t meth
  method writeFileSync : js_string t -> js_string t -> unit meth
  method existsSync : js_string t -> bool t meth
  method accessSync : js_string t -> number t -> unit meth
  method accessSync_prop : 'a optdef_prop
  method _F_OK_ : number t readonly_prop
  method _R_OK_ : number t readonly_prop
  method _W_OK_ : number t readonly_prop
end

let js_process : js_process t = Js.Unsafe.global##.process
let js_fs : js_fs t = NodeUtils.require "fs"

let print msg =
  js_process##.stdout##write (Js.string msg)

let command_line_args =
  let arr = Js.to_array js_process##.argv in
  if Array.length arr < 1 then
    [| |]
  else
    Array.map (fun s -> Js.to_string s) (Array.sub arr 1 (Array.length arr - 1))

let read_file path =
  let buf = (js_fs##readFileSync (Js.string path)) in
  Js.to_string (buf##toString)

let write_file path str =
  js_fs##writeFileSync (Js.string path) (Js.string str)

let get_env var =
  Optdef.case (Js.Unsafe.get js_process##.env var)
    (fun () -> None)
    (fun e  -> Some (Js.to_string e))

let open_app file_or_url =
  let node_open = NodeUtils.require "open" in
  Js.Unsafe.fun_call node_open [| Js.Unsafe.inject (Js.string file_or_url) |]

let exists_file file =
  try
    begin match Optdef.to_option js_fs##.accessSync_prop with
    | Some m ->
        js_fs##accessSync (Js.string file) (js_fs##._F_OK_);
        true
    | None ->
        Js.to_bool (js_fs##existsSync (Js.string file))
    end
  with
  | Js.Error e -> false

let chdir path = js_process##chdir (Js.string path)

let now () =
  let d = new%js Js.date_now in
  let date = Js.to_string (d##toLocaleDateString) in
  let time = Js.to_string (d##toLocaleTimeString) in
  Printf.sprintf "%s %s" date time

