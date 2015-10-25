
module type Cipher = sig
  type t

  type cipher_spec =
    | AES_CBC

  val create_cipher : cipher_spec -> Base64.t -> Base64.t -> t Result.t
  val encrypt : t -> string -> Base64.t
  val decrypt : t -> Base64.t -> string

  val gen_nonce : unit -> Base64.t
end

module StringMap = Map.Make(String)

module type HttpServer = sig
  type httpserver
  type buffer

  type raw_request = {
    hr_method  : string;
    hr_body    : string;
    hr_url     : string;
    hr_headers : string StringMap.t;
  }

  val create_server   : (httpserver -> raw_request -> (int -> string -> buffer -> unit) -> unit) -> httpserver
  val start_server    : httpserver -> port:int -> host:string -> callback:(unit -> unit) -> err_callback:(string -> unit) -> unit
  val stop_server     : httpserver -> unit

  val buffer_of_string : string -> buffer
  val buffer_of_file   : string -> buffer
end

module type KeepassDb = sig
  type t

  type keepass_entry = {
    kp_uuid     : string;
    kp_title    : string option;
    kp_username : string option;
    kp_password : string option;
    kp_url      : string option;
  }

  val open_db : string -> ?password:string option -> ?keyfile:string option
    -> (t -> string option -> unit)
    -> unit

  val save_db : t -> string -> (t -> string option -> unit) -> unit

  val get_entries : ?path:string list -> t -> keepass_entry array
  val create_entry : t -> string list -> keepass_entry -> unit

  val dump_db : t -> string

end

module type UrlUtil = sig
  val get_host_name : string -> string option
end

module type Sys = sig
  val print : string -> unit
  val read_file : string -> string
  val write_file : string -> string -> unit
  val command_line_args : string array
  val get_env : string -> string option
  val open_app : string -> unit
  val exists_file : string -> bool
end

module type Interface = sig
  module Cipher : Cipher
  module HttpServer : HttpServer
  module KeepassDb : KeepassDb
  module UrlUtil : UrlUtil
  module Sys : Sys
end

module Js : Interface = struct
  module Cipher = Cipher_js
  module HttpServer = HttpServer_js
  module KeepassDb = KeepassDb_js
  module UrlUtil = UrlUtil_js
  module Sys = Sys_backend_js
end
