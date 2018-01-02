
module type Interface = sig

  include module type of KeepassHttpServerTypes

  type t

  val create_server : (t -> request -> (response -> unit) -> unit) -> t
  val start_server  : port:int -> host:string -> ?callback:(unit -> unit) -> ?err_callback:(string -> unit) -> t -> unit
  val stop_server   : t -> unit

end

module Make(Backend : Backends.Interface) : Interface  = struct

  include KeepassHttpServerTypes

  module StringMap  = Backends.StringMap
  module HttpServer = Backend.HttpServer

  type t = Backend.HttpServer.httpserver

  let find_string_val ?(default=None) key keyvals =
    match (List.assoc key keyvals, default) with
    | (`String v, _)  -> MyResult.Ok v
    | (_, Some(v)) -> MyResult.Ok v
    | _ ->
        MyResult.Error (Printf.sprintf "property '%s' is not string" key)
    | exception Not_found ->
        begin match default with
        | None -> MyResult.Error (Printf.sprintf "key : '%s' is not found." key)
        | Some(v) -> MyResult.Ok v
        end

  let create_auth_req keyvals =
    let open MyResult.Monad in
    find_string_val ~default:(Some "") "Id" keyvals >>= fun id ->
    find_string_val "Nonce" keyvals >>= fun nonce ->
    find_string_val "Verifier" keyvals >>= fun verifier ->
    return {
      req_id = id;
      req_nonce = Base64.of_string nonce;
      req_verifier = Base64.of_string verifier; }

  let parse_request_json (json:Yojson.Safe.json) =
    let open MyResult.Monad in
    match json with
    | `Assoc keyvals ->
        let res_or_error =
          begin match find_string_val "RequestType" keyvals with
          | MyResult.Ok "test-associate" ->
              create_auth_req keyvals >>= fun auth_req ->
              return (ReqTestAssociate auth_req)
          | MyResult.Ok "associate" ->
              find_string_val "Key" keyvals >>= fun key ->
              return (ReqAssociate { req_key = Base64.of_string key })
          | MyResult.Ok "get-logins" ->
              create_auth_req keyvals >>= fun auth_req ->
              find_string_val "Url" keyvals >>= fun url ->
              return (ReqGetLogins (auth_req, { req_url = Base64.of_string url; }))
          | MyResult.Ok "get-logins-count" ->
              create_auth_req keyvals >>= fun auth_req ->
              find_string_val "Url" keyvals >>= fun url ->
              return (ReqGetLoginsCount (auth_req, { req_url = Base64.of_string url; }))
          | MyResult.Ok "set-login" ->
              create_auth_req keyvals >>= fun auth_req ->
              find_string_val "SubmitUrl" keyvals >>= fun submit_url ->
              find_string_val "Login" keyvals >>= fun login ->
              find_string_val "Password" keyvals >>= fun password ->
              let set_login_req = {
                req_submit_url = Base64.of_string submit_url;
                req_login = Base64.of_string login;
                req_password = Base64.of_string password;
              } in
              return (ReqSetLogin (auth_req, set_login_req))
          | MyResult.Ok req_type ->
              MyResult.Error ("Unknown RequestType : " ^ req_type)
          | MyResult.Error msg ->
              MyResult.Error msg
          end in
        begin match res_or_error with
        | MyResult.Ok res -> res
        | MyResult.Error err ->
            Logger.error err;
            ReqInvalid
        end
    | _ ->
        ReqInvalid

  let parse_request http_req =
    let open HttpServer in
    match http_req.hr_method with
    | "POST" ->
        begin try
          let json =  Yojson.Safe.from_string http_req.hr_body in
          parse_request_json json
        with
        | Yojson.Json_error msg ->
            Logger.error msg;
            ReqInvalid
        end
    | meth ->
        Logger.error ("Invalied http request method : " ^ meth);
        ReqInvalid

  let build_entries entries =
    List.map (fun entry ->
      `Assoc [
        ("Name", `String (Base64.to_string entry.entry_name));
        ("Login", `String (Base64.to_string entry.entry_login));
        ("Password", `String (Base64.to_string entry.entry_password));
        ("Uuid", `String (Base64.to_string entry.entry_uuid)); ]
    ) entries

  let build_common_res common =
    [("Success", `Bool common.res_success);
     ("Nonce", `String (Base64.to_string common.res_nonce));
     ("Verifier", `String (Base64.to_string common.res_verifier));]

  let response_to_json res =
    let assoc_list = match res with
    | ResTestAssociate (common,detail) ->
        let common_res = build_common_res common in
        ("Id", `String detail.res_id) :: common_res
    | ResAssociate (common,detail) ->
        let common_res = build_common_res common in
        ("Id", `String detail.res_id) :: common_res
    | ResGetLogins (common,detail) ->
        let common_res = build_common_res common in
        ("Id", `String detail.res_id) ::
        ("Entries", `List (build_entries detail.res_entries)) ::
        common_res
    | ResGetLoginsCount (common,detail) ->
        let common_res = build_common_res common in
        ("Count", `Int detail.res_count) :: common_res
    | ResSetLogin (common) ->
        build_common_res common
    | ResFailed ->
        [("Success", `Bool false)]
    in
    `Assoc assoc_list

  let make_http_hander keepass_http_handler = (fun server http_req send_res ->
    HttpServer.(
      Logger.debug "[KeepssHttp] HTTP request received.";
      Logger.debug (Printf.sprintf "  method : %s" http_req.hr_method);
      Logger.debug (Printf.sprintf "  url    : %s" http_req.hr_url);
      Logger.verbose                 "  headers:";
      StringMap.iter (fun k v ->
        Logger.verbose (Printf.sprintf "    %s : %s" k v)) http_req.hr_headers;
      Logger.debug (Printf.sprintf "  body   :\n%s" http_req.hr_body);
    );
    let request = parse_request http_req in
    let write_res res =
      let response_json = response_to_json res in
      let res_string = Yojson.Safe.to_string response_json in
      Logger.debug (Printf.sprintf "[KeepssHttp] send HTTP response:\n%s" res_string);
      send_res 200 "application/json" (HttpServer.buffer_of_string res_string)
    in
    keepass_http_handler server request write_res)

  let create_server handler =
    HttpServer.create_server (make_http_hander handler)

  let start_server ~port ~host ?(callback=(fun () -> ())) ?(err_callback=(fun _ -> ())) server =
    Logger.info (Printf.sprintf "[KeepssHttp] started at %s:%d." host port);
    HttpServer.start_server ~port ~host ~callback ~err_callback server

  let stop_server server =
    HttpServer.stop_server server

end
