open Js

class type js_buffer = object end

class type js_dechiper = object
  method update : js_string t -> js_string t -> js_string t -> js_string t meth
  method final  : js_string t -> js_string t meth
end

class type js_chiper = object
  method update : js_string t -> js_string t -> js_string t -> js_string t meth
  method final  : js_string t -> js_string t meth
end

class type js_ctypto = object
  method createCipheriv   : js_string t -> js_buffer t -> js_buffer t -> js_chiper t meth
  method createDecipheriv : js_string t -> js_buffer t -> js_buffer t -> js_dechiper t meth
end

let js_ctypto : js_ctypto t = NodeUtils.require "crypto"
let js_base64 = Js.string "base64"
let js_utf8   = Js.string "utf8"

let js_Buffer : (js_string t -> js_string t -> js_buffer t) constr = Js.Unsafe.variable "Buffer"

type cipher_spec =
  | AES_CBC

type t = {
  spec_name : string;
  key : Base64.t;
  iv : Base64.t;
}

let js_buffer_of_b64 b64 =
  jsnew js_Buffer (Js.string (Base64.to_string b64), js_base64)

let spec_to_string spec key =
  match (spec,String.length (Base64.decode key)) with
  | (AES_CBC,16) -> Result.Ok "aes-128-cbc"
  | (AES_CBC,24) -> Result.Ok "aes-192-cbc"
  | (AES_CBC,32) -> Result.Ok "aes-256-cbc"
  | (AES_CBC,_)  -> Result.Error "can't get cipher spec. invalid key length."

let create_cipher spec key iv =
  let open Result.Monad in
  spec_to_string spec key >>= fun spec_name ->
  return { spec_name; key; iv; }

let create_js_cipher {spec_name;key;iv} =
  js_ctypto##createCipheriv ((Js.string spec_name),(js_buffer_of_b64 key),(js_buffer_of_b64 iv))

let create_js_decipher {spec_name;key;iv} =
  js_ctypto##createDecipheriv ((Js.string spec_name),(js_buffer_of_b64 key),(js_buffer_of_b64 iv))

let encrypt cipher msg =
  let js_cipher = create_js_cipher cipher in
  let s1 = Js.to_string (js_cipher##update (Js.string msg, js_utf8, js_base64)) in
  let s2 = Js.to_string (js_cipher##final (js_base64)) in
  Base64.of_string (s1 ^ s2)

let decrypt cipher msg =
  let js_dechiper = create_js_decipher cipher in
  let s1 = Js.to_string (js_dechiper##update (Js.string (Base64.to_string msg), js_base64, js_utf8)) in
  let s2 = Js.to_string (js_dechiper##final (js_utf8)) in
  s1 ^ s2

let gen_nonce () =
  let now : Js.date Js.t = jsnew Js.date_now () in
  Random.init (now##getMilliseconds ());
  let s = String.init 16 (fun _ ->
    let i = match (Random.int (26 + 26 + 10)) with
    | n when n < 26      -> (int_of_char 'a') + n
    | n when n < 26 + 26 -> (int_of_char 'A') + n - 26
    | n                  -> (int_of_char '0') + n - 26 - 26 in
    char_of_int i) in
  Base64.encode s

