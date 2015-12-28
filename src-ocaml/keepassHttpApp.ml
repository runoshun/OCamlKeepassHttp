
module Make(Backend : Backends.Interface) : sig

  type app
  type app_config = AppConfig.t

  val start_app : (UiServerTypes.action -> unit) -> app_config -> string option ->
                   err_callback:(string -> unit) -> callback:(unit -> unit) -> app
  val stop_app  : app -> unit

end = struct

  module Provider = KeepassProvider.Make(Backend)
  module KeepassHttpServer = KeepassHttpServer.Make(Backend)

  module KeepassDb = Backend.KeepassDb
  module Cipher = Backend.Cipher
  module UrlUtil = Backend.UrlUtil
  module Sys = Backend.Sys

  type app = KeepassHttpServer.t
  type app_config = AppConfig.t

  let verify cipher iv verifier =
    Base64.to_string iv = (Cipher.decrypt cipher verifier)

  let gen_req_cipher provider request_auth =
    let open KeepassHttpServer in
    let open Cipher in
    let open Result.Monad in
    let nonce = request_auth.req_nonce in
    Provider.get_client_key provider request_auth.req_id >>= fun key ->
      create_cipher AES_CBC key nonce >>= fun cipher ->
        if verify cipher request_auth.req_nonce request_auth.req_verifier then
          return cipher
        else
          Result.Error "authentication of client is failed."

  let gen_response_cipher client_id key =
    let open Result.Monad in
    let open Cipher in
    let nonce = gen_nonce () in
    create_cipher AES_CBC key nonce >>= fun cipher ->
      let verifier = encrypt cipher (Base64.to_string nonce) in
      return (cipher, nonce, verifier)

  let gen_res_common provider req_auth =
    let open KeepassHttpServer in
    let open Result.Monad in
    let id = req_auth.req_id in
    Provider.get_client_key provider id >>= fun key ->
      gen_response_cipher id key >>= fun (cipher,nonce,verifier) ->
        return (cipher, { res_success = true;
                          res_nonce = nonce;
                          res_verifier = verifier })

  let to_login_entries cipher keepass_entries =
    let open KeepassDb in
    let open KeepassHttpServer in
    let encrypt s = match s with
                    | Some s -> Cipher.encrypt cipher s
                    | None   -> Base64.of_string "" in
    List.map (fun le ->
      { entry_name     = encrypt le.kp_title;
        entry_uuid     = encrypt (Some le.kp_uuid);
        entry_login    = encrypt le.kp_username;
        entry_password = encrypt le.kp_password; })
    keepass_entries

  let search_by_url url =
    (* TODO: more inteligent search *)
    let open KeepassDb in
    let web_page_host = UrlUtil.get_host_name url in
    (fun e ->
      let entry_host = match e.kp_url with Some u -> UrlUtil.get_host_name u | None -> None in
      match (web_page_host, entry_host) with
      | (Some w_host, Some e_host) -> w_host = e_host
      | (None, _) -> Logger.debug "[KeepssHttp] can't determine the web page's host. no entry returned."; false
      | (Some w_host, None) -> Logger.debug "[KeepssHttp] can't get entry url's host."; false
    )

  let make_app post_action config = KeepassHttpServer.create_server (fun _ req send_res ->
    let open Provider in
    let open KeepassHttpServer in
    let open Result.Monad in
    let with_send_res res =
      match res with
      | Result.Ok res -> send_res res
      | Result.Error msg ->
          Logger.error ("[KeepssHttp] " ^ msg);
          send_res ResFailed in
    with_provider config (fun provider_or_error ->
      begin match req with
        | ReqTestAssociate (req_auth) ->
            with_send_res begin
              provider_or_error >>= fun provider ->
              gen_req_cipher provider req_auth >>= fun _ ->
              gen_res_common provider req_auth >>= fun (_,res_common) ->
              return (ResTestAssociate (res_common, { res_id = req_auth.req_id }))
            end

        | ReqAssociate (req_assoc) ->
            let key = req_assoc.req_key in
            let perform data callback =
              begin match data with
              | `String new_client_id ->
                  begin match
                    provider_or_error >>= fun provider ->
                    if new_client_id = "" then
                      Result.Error "client id can not be empty."
                    else
                      Result.Ok new_client_id >>= fun new_client_id ->
                    gen_response_cipher new_client_id key >>= fun (_,nonce,verifier) ->
                    let res_common = {
                      res_success = true;
                      res_nonce = nonce;
                      res_verifier = verifier } in
                    let res_assoc = { res_id = new_client_id; } in
                    create_client_config provider ~client_id:new_client_id ~client_key:key;
                    save provider;
                    return (ResAssociate (res_common,res_assoc))
                  with
                  | Result.Ok res ->
                      callback (Result.Ok "successfully associated");
                      send_res res
                  | Result.Error e ->
                      callback (Result.Error e);
                      send_res ResFailed
                  end
              | _ ->
                  callback (Result.Error "invalid data");
                  send_res ResFailed
              end in
            post_action UiServerTypes.{
              act_id = Uuid.gen_v4 ();
              act_type = ActTypeAssociate (Base64.to_string key);
              act_date = Sys.now ();
              act_perform = perform;
            }

        | ReqGetLogins (req_auth, req_get_logins) ->
            with_send_res begin
              provider_or_error >>= fun provider ->
              gen_req_cipher provider req_auth >>= fun req_cipher ->
              gen_res_common provider req_auth >>= fun (res_cipher,res_common) ->
              let url = Cipher.decrypt req_cipher req_get_logins.req_url in
              let kp_entries = search_entries provider (search_by_url url) in
              let res_get_logins = {
                res_id = req_auth.req_id;
                res_entries = to_login_entries res_cipher kp_entries; } in
              return (ResGetLogins (res_common, res_get_logins))
            end

        | ReqGetLoginsCount (req_auth, req_get_logins) ->
            with_send_res begin
              provider_or_error >>= fun provider ->
              gen_req_cipher provider req_auth >>= fun req_cipher ->
              gen_res_common provider req_auth >>= fun (_,res_common) ->
              let url = Cipher.decrypt req_cipher req_get_logins.req_url in
              let kp_entries = search_entries provider (search_by_url url) in
              let res_login_count = { res_count = List.length kp_entries } in
              return (ResGetLoginsCount (res_common, res_login_count))
            end

        | ReqSetLogin (req_auth, req_set_login) ->
            with_send_res begin
              provider_or_error >>= fun provider ->
              gen_req_cipher provider req_auth >>= fun req_cipher ->
              gen_res_common provider req_auth >>= fun (_,res_common) ->
              let submit_url = Cipher.decrypt req_cipher req_set_login.req_submit_url in
              let login      = Cipher.decrypt req_cipher req_set_login.req_login in
              let password   = Cipher.decrypt req_cipher req_set_login.req_password in
              create_login provider submit_url login password;
              return (ResSetLogin res_common)
            end
        | ReqInvalid ->
            Logger.debug "[KeepssHttp] Invalid request received";
            with_send_res (return ResFailed)
      end))

  let start_app post_action app_config pass ~err_callback ~callback =
    let open AppConfig in
    let keyfile = match app_config.keepass_db_keyfile with
    | "" -> None
    | s -> Some s
    in
    let provider_config =
      Provider.make_config
        ~keyfile:keyfile
        ~password:pass
        app_config.keepass_db in
    let app = make_app post_action provider_config in
    KeepassHttpServer.start_server
      ~port:app_config.httpserver_port
      ~host:app_config.httpserver_host
      ~err_callback
      ~callback
      app;
    app

  let stop_app app = KeepassHttpServer.stop_server app

end

