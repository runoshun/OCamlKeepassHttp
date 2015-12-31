(*let console_log (s:string) =*)
  (*(Js.Unsafe.variable "console")##log (Js.string s) |> ignore*)

(*let printfn fmt =*)
  (*Printf.ksprintf (fun s -> console_log s) fmt*)

let debug_js_obj a =
  Js.Unsafe.global##.__debugobj := a;
  Js.Unsafe.eval_string "console.log(_debugobj)"

let require m =
  Js.Unsafe.eval_string ("require('" ^ m ^ "')")

let is_array obj =
  Js.to_bool ((Js.Unsafe.variable "Array")##isArray obj)

let argv n =
  Js.to_string (Js.Unsafe.eval_string (Printf.sprintf "process.argv[%d]" n))

