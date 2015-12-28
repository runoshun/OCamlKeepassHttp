
module type Interface = sig

  open UiServerTypes

  type t

  val create_server : (t -> request -> ('a response -> unit) -> unit) -> t
  val start_server : port:int -> host:string -> ?callback:(unit -> unit) -> ?err_callback:(string -> unit) -> t -> unit
  val stop_server : t -> unit

end

module Make(Backend:Backends.Interface) : Interface = struct

  open UiServerTypes

  type t = Backend.HttpServer.httpserver

  module HttpServer = Backend.HttpServer
  module StringMap = Map.Make(String)


  let parse_http_request http_req =
    let open HttpServer in
    Logger.debug "[UiServer] receive request :";
    Logger.debug (Printf.sprintf "  method : %s" http_req.hr_method);
    Logger.debug (Printf.sprintf "  url    : %s" http_req.hr_url);
    Logger.verbose                 "  headers:";
    StringMap.iter (fun k v ->
        Logger.verbose (Printf.sprintf "    %s : %s" k v)) http_req.hr_headers;
    Logger.debug (Printf.sprintf "  body   :\n%s" http_req.hr_body);
    match http_req.hr_method with
    | "GET" when http_req.hr_url = "/state" ->
        ReqGetState
    | "GET"  ->
        ReqGetStatic http_req.hr_url
    | "POST" when http_req.hr_url = "/state" ->
        ReqPostConfig (http_req.hr_body)
    | "POST" when http_req.hr_url = "/restart_keepass_http" ->
        ReqPostRestartKeepassHttpServer
    | "POST" when http_req.hr_url = "/action" ->
        ReqPostAction (http_req.hr_body)
    | "POST" when http_req.hr_url = "/delete_action" ->
        ReqPostDeleteAction (http_req.hr_body)
    | meth   ->
        ReqInvalid

  let json_of_actions actions =
    let to_json action =
      let common = [
        ("date", `String action.act_date);
        ("id",   `String (Uuid.to_string action.act_id));
      ] in
      match action.act_type with
      | ActTypeAssociate(client) ->
          `Assoc (
            ("type", `String "associate") ::
            ("client", `String client) ::
            common
          )
    in
    `List (List.map to_json actions)

  let string_of_state state =
    let config = AppConfig.to_json !(state.config) in
    let password = match !(state.password) with None -> "" | Some p -> p in
    let server_running = match !(state.server) with None -> false | Some _ -> true in
    let json = `Assoc [
      ("config", config);
      ("password", `String password);
      ("server_running", `Bool server_running);
      ("actions", json_of_actions !(state.actions));
    ] in
    Yojson.Safe.to_string json

  let content_type_map = [
    ("css", "text/css");
    ("html", "text/html");
    ("js", "text/javascript");
    ("png", "image/png");
    ("jpg", "image/jpeg");
    ("jpeg", "image/jpeg");
    ("ttf", "application/x-font-truetype");
    ("woff", "application/font-woff");
    ("woff2", "application/font-woff2");
  ]

  let get_ext path =
    try
      let len = String.length path in
      let idx = String.rindex path '.' + 1 in
      String.sub path idx (len - idx)
    with
    | _ -> ""

  let build_response res =
    let buffer_of_string = HttpServer.buffer_of_string in
    match res with
    | ResGetStatic(rel_path) ->
        Logger.debug (Printf.sprintf "[UiServer] ResGetStatic, path = %s" rel_path);
        let ext = get_ext rel_path in
        let content_type = try List.assoc ext content_type_map with
        | _ -> "application/octet-stream" in
        begin try
          let buf = HttpServer.buffer_of_file rel_path in
          (200,content_type,buf)
        with
        | e -> (404,"text/plain", buffer_of_string "")
        end
    | ResGetState(state) ->
        let state = string_of_state state in
        Logger.debug (Printf.sprintf "[UiServer] ResGetState, state = %s" state);
        (200,"application/json",buffer_of_string state)
    | ResSuccess ->
        Logger.debug (Printf.sprintf "[UiServer] ResSuccess");
        (200, "application/json", buffer_of_string "{}")
    | ResError(error) ->
        Logger.debug (Printf.sprintf "[UiServer] ResError, error = %s" error);
        (200, "application/json", buffer_of_string (Printf.sprintf "{ \"error\" : \"%s\" }" error))
    | ResInvalid(error) ->
        Logger.debug (Printf.sprintf "[UiServer] ResInvalid error = %s" error);
        (400, "text/plain", (buffer_of_string error))
    | ResNotFound ->
        Logger.debug (Printf.sprintf "[UiServer] ResNotFound");
        (404, "", buffer_of_string "")

  let make_http_hander handler = (fun server http_req send_res ->
    let request = parse_http_request http_req in
    let write_res res =
      let (status, content_type, res_string) = build_response res in
      Logger.debug (Printf.sprintf "[UiServer] send HTTP response, status = %d" status);
      send_res status content_type res_string
    in
    handler server request write_res)

  let create_server handler =
    HttpServer.create_server (make_http_hander handler)

  let start_server ~port ~host ?(callback=(fun () -> ())) ?(err_callback=(fun _ -> ())) server =
    HttpServer.start_server ~port ~host ~callback ~err_callback server

  let stop_server server =
    HttpServer.stop_server server

end
