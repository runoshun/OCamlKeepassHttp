
module type Interface = sig
  type t
  type provider_config
  type keepass_entry

  val make_config    : password:string option -> keyfile:string option -> string -> provider_config
  val with_provider  : provider_config -> (t MyResult.t -> unit) -> unit

  val get_client_key : t -> string -> Base64.t MyResult.t
  val search_entries : t -> ?path:string list -> (keepass_entry -> bool) -> keepass_entry list
  val create_login   : t -> url:string -> login:string -> password:string -> unit
  val create_client_config : t -> client_id:string -> client_key:Base64.t -> unit
  val save           : t -> unit
end

module Make(Backend : Backends.Interface) :
  Interface with type keepass_entry := Backend.KeepassDb.keepass_entry =
struct

  module KeepassDb = Backend.KeepassDb
  module UrlUtil   = Backend.UrlUtil

  let logins_path = ["Node-Keepass"; "Logins"]
  let clients_path = ["Node-Keepass"; "Authorized clients";]

  type t = {
    db : KeepassDb.t;
    db_path : string;
    dirty : bool ref;
  }

  type provider_config = {
    db_path : string;
    password : string option;
    keyfile : string option;
  }

  let make_config ~password ~keyfile db_path =
    let password = match password with Some "" -> None | Some s -> Some s | None -> None in
    let keyfile = match keyfile with Some "" -> None | Some s -> Some s | None -> None in
    { db_path; password; keyfile; }

  let search_entries provider ?(path=[]) search_f =
    let entries = Array.to_list (KeepassDb.get_entries ~path provider.db) in
    List.filter search_f entries

  let create_login provider ~url ~login ~password =
    let open KeepassDb in
    KeepassDb.create_entry provider.db logins_path
      { kp_uuid     = Base64.to_string (Uuid.gen_v4_hex_base64 ());
        kp_url      = Some url;
        kp_username = Some login;
        kp_password = Some password;
        kp_title    = UrlUtil.get_host_name url };
    provider.dirty := true

  let create_client_config provider ~client_id ~client_key =
    let open KeepassDb in
    KeepassDb.create_entry provider.db clients_path
      { kp_uuid     = Base64.to_string (Uuid.gen_v4_hex_base64 ());
        kp_url      = None;
        kp_username = None;
        kp_password = Some (Base64.to_string client_key);
        kp_title    = Some client_id };
    provider.dirty := true

  let get_client_key provider client_id =
    let open KeepassDb in
    let entries = search_entries provider ~path:clients_path (fun e -> e.kp_title = (Some client_id)) in
    if List.length entries = 0 then
      MyResult.Error ("no client key found. client id = " ^ client_id)
    else
      match (List.hd entries).kp_password with
      | Some pass -> MyResult.Ok (Base64.of_string pass)
      | None      -> assert false

  let save provider =
    if !(provider.dirty) then begin
      Logger.debug (Printf.sprintf "[Provider] keepass database '%s' is saved." provider.db_path);
      KeepassDb.save_db provider.db provider.db_path (fun _ _ -> ())
    end else
      Logger.debug (Printf.sprintf "[Provider] keepass database '%s' is not dirty, skip saving." provider.db_path)

  let with_provider cfg thunk =
    try KeepassDb.open_db cfg.db_path ~password:cfg.password ~keyfile:cfg.keyfile
      (fun db err ->
         match err with
         | Some e -> thunk (MyResult.Error e)
         | None   ->
             let provider = {
               db = db;
               db_path = cfg.db_path;
               dirty = ref false;
             } in
             Logger.debug (Printf.sprintf "[Provider] keepass database '%s' is opened." cfg.db_path);
             (*Logger.debug (KeepassDb.dump_db db);*)
             thunk (MyResult.Ok provider);
             save provider)
    with
    | e -> thunk (MyResult.Error (Printexc.to_string e))

end
