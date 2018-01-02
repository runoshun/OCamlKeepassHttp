
module Make(Backend:Backends.Interface) = struct

  module UiApp = UiApp.Make(Backend)
  module KeepassHttpApp = KeepassHttpApp.Make(Backend)

  module Provider = KeepassProvider.Make(Backend)
  module Sys = Backend.Sys

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

  let with_check_db state thunk =
    let open AppConfig in
    let open UiServerTypes in
    let password = !(state.password) in
    let keyfile = match !(state.config).keepass_db_keyfile with "" -> None | s -> Some s in
    let db = !(state.config).keepass_db in
    let cfg = Provider.make_config ~password ~keyfile db in
    Provider.with_provider cfg (function
      | MyResult.Error e -> thunk (Some e)
      | MyResult.Ok _  -> thunk None)

  let show_ui_screen state =
    let config = !(state.UiServerTypes.config) in
    let url = Printf.sprintf "http://%s:%d/index.html"
                   config.AppConfig.configserver_host
                   config.AppConfig.configserver_port in
    Logger.info (Printf.sprintf "open %s" url);
    Sys.open_app url

  let make_restart_server state = fun post_action _done ->
    let open UiServerTypes in
    with_check_db state (function
    | Some e -> _done (Some e)
    | None ->
        begin match !(state.server) with
        | None -> ()
        | Some app -> KeepassHttpApp.stop_app app
        end;
        let err_callback err =
          state.server := None;
          _done (Some err)
        in
        let callback () = _done None in
        let app = KeepassHttpApp.start_app post_action !(state.config) !(state.password) ~err_callback ~callback in
        state.server := Some app;)

  let start app_config config_path =
    let open UiServerTypes in
    let state = {
      config = ref app_config;
      config_path = ref config_path;
      server = ref None;
      password = ref None;
      actions = ref [];
    } in
    let restart_server = make_restart_server state in
    UiApp.start_app state restart_server |> ignore;
    UiApp.open_ui state

  let parse_opts argv =
    let open Arg in
    let config = ref (Filename.concat home_dir ".node-keepass-http.conf") in
    let debug = ref 2 in
    let spec_list = [
      ("-c", Set_string config,  "set config file. default='~/.node-keepass-http.conf'");
      ("-v", Set_int debug,      "set log level. default=2");
    ] in
    Arg.parse_argv argv spec_list Sys.print "Keepass HTTP compatible server running on nodejs. Available options:";
    if Sys.exists_file !config then
      let config_string = try (Sys.read_file !config) with _ -> "{}" in
      (AppConfig.from_string config_string, !debug, !config)
    else
      (MyResult.Error "config file is not found", !debug, !config)

  let main exec_dir =
    Sys.chdir exec_dir;
    try
      let (app_config,log_level,config_path) = parse_opts Sys.command_line_args in
      Logger.set_log_level log_level;
      let app_config = begin match app_config with
      | MyResult.Error e ->
          Logger.info e;
          AppConfig.default
      | MyResult.Ok app_config -> app_config
      end in
      Logger.debug ("config = " ^ (AppConfig.to_string app_config));
      start app_config config_path
    with
    | Arg.Help msg -> Sys.print msg
    | Arg.Bad s    -> Sys.print s

end
