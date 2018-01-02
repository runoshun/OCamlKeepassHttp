type t = {
  httpserver_host              : string;
  httpserver_port              : int;
  keepass_db                   : string;
  keepass_db_keyfile           : string;

  configserver_host            : string;
  configserver_port            : int;

  disable_open_browser    : bool;
}

let key_host = "httpserver_host"
let key_port = "httpserver_port"
let key_db   = "keepass_db"
let key_keyfile = "keepass_db_keyfile"
let key_conf_host = "configserver_host"
let key_conf_port = "configserver_port"
let key_disable_open_browser = "disable_open_browser"

let default = {
  httpserver_host = "localhost";
  httpserver_port = 19455;
  keepass_db      = "keepass.kdbx";
  keepass_db_keyfile = "";
  configserver_host = "localhost";
  configserver_port = 18080;
  disable_open_browser = false;
}

let assoc_string key alist default =
  match List.assoc key alist with
  | `String s -> s
  | _ -> failwith (Printf.sprintf "'%s' must be string" key)
  | exception Not_found -> default

let assoc_int key alist default =
  match List.assoc key alist with
  | `Int i -> i
  | `String s -> (try int_of_string s with e -> default)
  | _ -> failwith (Printf.sprintf "'%s' must be int" key)
  | exception Not_found -> default

let assoc_bool key alist default =
  match List.assoc key alist with
  | `Bool b -> b
  | _ -> failwith (Printf.sprintf "'%s' must be bool" key)
  | exception Not_found -> default

let from_json ?(default=default) json =
  match json with
  | `Assoc values ->
      begin try
        MyResult.Ok {
          httpserver_host      = assoc_string key_host                 values default.httpserver_host;
          httpserver_port      = assoc_int    key_port                 values default.httpserver_port;
          keepass_db           = assoc_string key_db                   values default.keepass_db;
          keepass_db_keyfile   = assoc_string key_keyfile              values default.keepass_db_keyfile;
          configserver_host    = assoc_string key_conf_host            values default.configserver_host;
          configserver_port    = assoc_int    key_conf_port            values default.configserver_port;
          disable_open_browser = assoc_bool   key_disable_open_browser values default.disable_open_browser;
        }
      with
      | Failure e -> MyResult.Error e
      end
  | _ ->
      MyResult.Error "app_config parse error: invalid format json."

let from_string ?default str =
  match Yojson.Basic.from_string str with
  | json -> from_json ?default json
  | exception (Yojson.Json_error msg) ->
      MyResult.Error ("app_config parse error: " ^ msg)

let to_json t =
  let open Yojson.Safe in
  let v = [
    (key_host,    `String t.httpserver_host);
    (key_port,    `Int t.httpserver_port);
    (key_db,      `String t.keepass_db);
    (key_keyfile, `String t.keepass_db_keyfile);
    (key_conf_host, `String t.configserver_host);
    (key_conf_port, `Int t.configserver_port);
    (key_disable_open_browser, `Bool t.disable_open_browser)
  ] in
  `Assoc v

let to_string ?(pretty=false) t =
  if pretty then
    Yojson.Safe.pretty_to_string (to_json t)
  else
    Yojson.Safe.to_string (to_json t)

