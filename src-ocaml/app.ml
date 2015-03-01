
module Make(Backend : Backends.Interface) : sig

  val main : unit -> unit

end = struct

  module Provider = KeepassProvider.Make(Backend)
  module KeepassHttpServer = KeepassHttpServer.Make(Backend)
  module UserPrompt = UserPrompt.MakeHttpPrompt(Backend)

  module KeepassDb = Backend.KeepassDb
  module Cipher = Backend.Cipher
  module UrlUtil = Backend.UrlUtil
  module Sys = Backend.Sys

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
      | (None, _) -> Logger.debug "can't determine the web page's host. no entry returned."; false
      | (Some w_host, None) -> Logger.debug "can't get entry url's host."; false
    )

  let make_app config = KeepassHttpServer.create_server (fun _ req send_res ->
    let open Provider in
    let open KeepassHttpServer in
    let open Result.Monad in
    let with_send_res res =
      match res with
      | Result.Ok res -> send_res res
      | Result.Error msg ->
          Logger.error msg;
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
            let options = {
              UserPrompt.message = Printf.sprintf "New client associate request received. key : %s"
                                     (Base64.to_string key);
              UserPrompt.input_password = false;
            } in
            UserPrompt.prompt ~options (fun new_client_id ->
              with_send_res begin
                provider_or_error >>= fun provider ->
                new_client_id >>= fun new_client_id ->
                gen_response_cipher new_client_id key >>= fun (_,nonce,verifier) ->
                let res_common = {
                  res_success = true;
                  res_nonce = nonce;
                  res_verifier = verifier } in
                let res_assoc = { res_id = new_client_id; } in
                create_client_config provider ~client_id:new_client_id ~client_key:key;
                save provider;
                return (ResAssociate (res_common,res_assoc))
              end)

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
            Logger.debug "Invalid request received";
            with_send_res (return ResFailed)
      end))

  let start_sever app_config pass =
    let open AppConfig in
    let provider_config =
      Provider.make_config
        ~keyfile:app_config.keepass_db_keyfile
        ~password:pass
        app_config.keepass_db in
    let app = make_app provider_config in
    KeepassHttpServer.start_server
      ~port:app_config.httpserver_port
      ~host:app_config.httpserver_host
      app

  let read_password app_config thunk =
    let open AppConfig in
    let open Result.Monad in
    match app_config.keepass_db_password_required with
    | Some true ->
        let options = {
          UserPrompt.message = "Input keepass DB password";
          UserPrompt.input_password = true
        } in
        UserPrompt.prompt ~options (fun password_or_error ->
          begin match password_or_error with
          | Result.Ok password -> thunk (Some password)
          | Result.Error e -> Logger.error ("get password failed : " ^ e); thunk None
          end)
    | None | Some false ->
        thunk None

  let home_dir =
    match Sys.get_env "HOME" with
    | Some h -> h
    | None ->
        begin match Sys.get_env "USERPROFILE" with
        | Some u -> u
        | None ->
            Logger.info "can't determine the home directory. using current dir for base dir.";
            "."
        end

  let parse_opts argv =
    let open Arg in
    let config = ref "" in
    let db = ref (Filename.concat home_dir "keepass.kdbx") in
    let debug = ref 2 in
    let host = ref "" in
    let port = ref (-1) in
    let req_pass = ref false in
    let keyfile = ref "" in
    let spec_list = [
      ("-f", Set_string config, "read specified file as config file.");
      ("-d", Set_string db,     "set keepass db file. default='keepass.kdbx'");
      ("-h", Set_string host,   "set http server hostname. default='127.0.0.1'");
      ("-p", Set_int port,      "set http server hostname. default=19455");
      ("-k", Set_string keyfile,"set keepass db keyfile path.");
      ("-r", Set req_pass,      "if set, keepass db password prompt is shown in startup.");
      ("-v", Set_int debug,     "set log level. default=2");
    ] in
    Arg.parse_argv argv spec_list Sys.print "Keepass HTTP compatible server running on nodejs. Available options:";
    if !config <> "" then
      let app_config = AppConfig.from_string (Sys.read_file !config) in
      (app_config, !debug)
    else
      let app_config = AppConfig.{
        httpserver_host = if !host = "" then None else Some !host;
        httpserver_port = if !port = -1 then None else Some !port;
        keepass_db = !db;
        keepass_db_keyfile = if !keyfile = "" then None else Some !keyfile;
        keepass_db_password_required = Some !req_pass;
      } in
      (Result.Ok app_config, !debug)

  let main () =
    try
      let (app_config,log_level) = parse_opts Sys.command_line_args in
      Logger.set_log_level log_level;
      begin match app_config with
      | Result.Error m -> Sys.print m; Sys.print "\n"
      | Result.Ok app_config ->
          read_password app_config (fun password -> start_sever app_config password)
      end
    with
    | Arg.Help msg -> Sys.print msg

end

