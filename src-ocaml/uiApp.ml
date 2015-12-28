
module type Interface = sig

  type app
  type 'a app_state

  val start_app : 'a app_state -> ((UiServerTypes.action -> unit) -> (string option -> unit) -> unit) -> app
  val stop_app  : app -> unit

end

module Make(Backend:Backends.Interface) : Interface with
 type 'a app_state := 'a UiServerTypes.app_state = struct

  module UiServerImpl = UiServer.Make(Backend)
  module Sys = Backend.Sys

  open UiServerTypes

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

  let string_as_json str =
    try Result.Ok (Yojson.Safe.from_string str) with
    | _ -> Result.Error "invalid json"

  let json_as_string json =
    match json with
    | `String s -> Result.Ok s
    | _ -> Result.Error "invalid json, expected string."

  let json_as_assoc json =
    match json with
    | `Assoc assoc -> Result.Ok assoc
    | _ -> Result.Error "invalid json, expected assoc."

  let json_find_assoc field assoc =
    match List.assoc field assoc with
    | json -> Result.Ok json
    | exception Not_found -> Result.Error (Printf.sprintf "field '%s' is not found." field)

  let find_action actions assoc =
    let open Result.Monad in
    json_find_assoc "id" assoc >>= fun id ->
    json_as_string id >>= fun id ->
    try return (List.find (fun act -> id = (Uuid.to_string (act.act_id))) actions) with
    | Not_found -> Result.Error "action is not found."

  let make_post_action app_state = fun action ->
    match action.act_type with
    | ActTypeAssociate _ ->
        if List.exists (fun act -> act.act_type = action.act_type) !(app_state.actions) then
          ()
        else
          let config = !(app_state.UiServerTypes.config) in
          Sys.open_app (Printf.sprintf "http://%s:%d/index.html"
                         config.AppConfig.configserver_host
                         config.AppConfig.configserver_port);
          app_state.actions := action :: !(app_state.actions)

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
    | ReqPostAction(body) ->
        Logger.debug ("[UiServer] ReqPostAction : ");
        Logger.debug ("  body : " ^ body);
        let open Result.Monad in
        begin match
          string_as_json body >>= fun json ->
          json_as_assoc json >>= fun assoc ->
          find_action !(app_state.actions) assoc >>= fun action ->
          json_find_assoc "data" assoc >>= fun data ->
          action.act_perform data (function
            | Result.Ok _ ->
                app_state.actions := List.filter ((<>) action) !(app_state.actions);
                send_res ResSuccess
            | Result.Error e -> send_res (ResInvalid e));
          return ()
        with
        | Result.Ok () -> ()
        | Result.Error e -> send_res (ResError e)
        end
    | ReqPostDeleteAction(body) ->
        Logger.debug ("[UiServer] ReqPostDeleteAction : ");
        Logger.debug ("  body : " ^ body);
        let open Result.Monad in
        begin match
          string_as_json body >>= fun json ->
          json_as_assoc json >>= fun assoc ->
          find_action !(app_state.actions) assoc >>= fun action ->
          app_state.actions := (List.filter ((<>) action) !(app_state.actions));
          return ()
        with
        | Result.Ok () -> send_res ResSuccess
        | Result.Error e -> send_res (ResError e)
        end
    | ReqPostRestartKeepassHttpServer ->
        Logger.debug ("[UiServer] ReqPostRestartKeepassHttpServer");
        begin try
          let open UiServerTypes in
          Sys.write_file !(app_state.config_path) (AppConfig.to_string ~pretty:true !(app_state.config));
          restart_server (make_post_action app_state) (function
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
