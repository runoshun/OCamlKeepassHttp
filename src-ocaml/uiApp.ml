
module type Interface = sig

  type app
  type 'a app_state

  val start_app : 'a app_state  -> ((string option -> unit) -> unit) -> app
  val stop_app  : app -> unit

end

module Make(Backend:Backends.Interface) : Interface with
 type 'a app_state := 'a UiServerTypes.app_state
 = struct

  module UiServerImpl = UiServer.Make(Backend)
  module Sys = Backend.Sys

  type app = UiServerImpl.t

  let state_of_body default body =
    try
      let json = Yojson.Safe.from_string body in
      Result.Monad.(
        AppConfig.from_json ~default json >>= fun config ->
        let password = match json with
        | `Assoc alist ->
            begin match List.assoc "password" alist with
            | `String p -> Some p
            | _ -> None
            | exception _ -> None
            end
        | _ -> None
        in
        return (config, password))
    with
    | e -> Result.Error (Printexc.to_string e)

  let make_app_handler app_state restart_server = fun server req send_res ->
    let open UiServerImpl in
    match req with
    | ReqGetStatic path ->
        Logger.debug ("[UiServer] ReqGetStatic : " ^ path);
        let path = "." ^ path in
        send_res (ResGetStatic path)
    | ReqGetState ->
        Logger.debug ("[UiServer] ReqGetState");
        send_res (ResGetState app_state)
    | ReqPostConfig(body) ->
        Logger.debug ("[UiServer] ReqPostConfig : ");
        Logger.debug ("  body : " ^ body);
        begin match state_of_body !(app_state.UiServerTypes.config) body with
        | Result.Ok(config, password) ->
            app_state.UiServerTypes.config := config;
            app_state.UiServerTypes.password := password;
            send_res ResSuccess
        | Result.Error e ->
            send_res (ResError e)
        end
    | ReqPostRestartKeepassHttpServer ->
        Logger.debug ("[UiServer] ReqPostRestartKeepassHttpServer");
        begin try
          (* save config to file *)
          let open UiServerTypes in
          Sys.write_file !(app_state.config_path) (AppConfig.to_string ~pretty:true !(app_state.config));
          (* restart server *)
          restart_server (function
            | Some e -> send_res (ResError e)
            | None   -> send_res (ResSuccess))
        with
        | e ->
            let error =(Printexc.to_string e) in
            Logger.error ("[UiServer] " ^ error);
            send_res (ResError error);
        end
    | ReqInvalid  ->
        Logger.debug ("[UiServer] ReqInvalid");
        send_res (ResInvalid "Invalid request received.")

  let start_app app_state restart_server =
    let app = UiServerImpl.create_server (make_app_handler app_state restart_server) in
    let port = !(app_state.UiServerTypes.config).AppConfig.configserver_port in
    let host = !(app_state.UiServerTypes.config).AppConfig.configserver_host in
    Logger.info (Printf.sprintf "[UiServer] started at %s:%d" host port);
    UiServerImpl.start_server ~port ~host app;
    app

  let stop_app app = UiServerImpl.stop_server app

end
