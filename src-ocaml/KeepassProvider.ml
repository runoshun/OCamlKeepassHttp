
module type ProviderInterface = sig
  type t
  type provider_config
  type keepass_entry

  val make_config    : ?password:string option -> ?keyfile:string option -> string -> provider_config
  val with_provider  : provider_config -> (t Result.t -> unit) -> unit

  val get_client_key : t -> string -> Base64.t Result.t
  val search_entries : t -> ?path:string list -> (keepass_entry -> bool) -> keepass_entry list
  val create_login   : t -> url:string -> login:string -> password:string -> unit
  val create_client_config : t -> client_id:string -> client_key:Base64.t -> unit
  val save           : t -> unit
end

module Make(Backend : Backends.Interface) :
  ProviderInterface with type keepass_entry := Backend.KeepassDb.keepass_entry =
struct

  module KeepassDb = Backend.KeepassDb
  module UrlUtil   = Backend.UrlUtil

  let logins_path = ["Node KeePass HTTP"; "Logins"]
  let clients_path = ["Node KeePass HTTP"; "Clients"]

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

  let make_config ?(password=None) ?(keyfile=None) db_path =
    { db_path; password; keyfile; }

  let search_entries provider ?(path=[]) search_f =
    let entries = Array.to_list (KeepassDb.get_entries ~path provider.db) in
    List.filter search_f entries

  let create_login provider ~url ~login ~password =
    let open KeepassDb in
    KeepassDb.create_entry provider.db logins_path
      { kp_uuid     = Uuid.gen_v4 ();
        kp_url      = Some url;
        kp_username = Some login;
        kp_password = Some password;
        kp_title    = UrlUtil.get_host_name url };
    provider.dirty := true

  let create_client_config provider ~client_id ~client_key =
    let open KeepassDb in
    KeepassDb.create_entry provider.db clients_path
      { kp_uuid     = Uuid.gen_v4 ();
        kp_url      = None;
        kp_username = None;
        kp_password = Some (Base64.to_string client_key);
        kp_title    = Some client_id };
    provider.dirty := true

  let get_client_key provider client_id =
    let open KeepassDb in
    let entries = search_entries provider ~path:clients_path (fun e -> e.kp_title = (Some client_id)) in
    if List.length entries = 0 then
      Result.Error ("no client key found. client id = " ^ client_id)
    else
      match (List.hd entries).kp_password with
      | Some pass -> Result.Ok (Base64.of_string pass)
      | None      -> assert false

  let save provider =
    if !(provider.dirty) then begin
      Logger.debug (Printf.sprintf "keepass database '%s' is saved." provider.db_path);
      KeepassDb.save_db provider.db provider.db_path (fun _ _ -> ())
    end else
      Logger.debug (Printf.sprintf "keepass database '%s' is not dirty, skip saving." provider.db_path)

  let with_provider cfg thunk =
    KeepassDb.open_db cfg.db_path ~password:cfg.password ~keyfile:cfg.keyfile
      (fun db err ->
         match err with
         | Some e -> thunk (Result.Error e)
         | None   ->
             let provider = {
               db = db;
               db_path = cfg.db_path;
               dirty = ref false;
             } in
             Logger.debug (Printf.sprintf "keepass database '%s' is opened." cfg.db_path);
             thunk (Result.Ok provider);
             save provider)

end
