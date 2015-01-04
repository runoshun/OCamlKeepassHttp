type t = {
  httpserver_host              : string option;
  httpserver_port              : int option;
  keepass_db                   : string;
  keepass_db_password_required : bool option;
  keepass_db_keyfile           : string option;
}

let assoc_string_exn key alist =
  match List.assoc key alist with
  | `String s -> s
  | _ -> failwith (Printf.sprintf "'%s' must be string" key)
  | exception Not_found -> failwith (Printf.sprintf "'%s' must be defined." key)

let assoc_int_opt_exn key alist =
  match List.assoc key alist with
  | `Int i -> Some i
  | _ -> failwith (Printf.sprintf "'%s' must be int" key)
  | exception Not_found -> None

let assoc_string_opt_exn key alist =
  match List.assoc key alist with
  | `String s -> Some s
  | _ -> failwith (Printf.sprintf "'%s' must be string" key)
  | exception Not_found -> None

let assoc_bool_opt_exn key alist =
  match List.assoc key alist with
  | `Bool b -> Some b
  | _ -> failwith (Printf.sprintf "'%s' must be bool" key)
  | exception Not_found -> None

let from_string str =
  match Yojson.Basic.from_string str with
  | `Assoc values ->
      begin try
        Result.Ok
        { httpserver_host              = assoc_string_opt_exn "httpserver_host" values;
          httpserver_port              = assoc_int_opt_exn    "httpserver_port" values;
          keepass_db                   = assoc_string_exn     "keepass_db" values;
          keepass_db_password_required = assoc_bool_opt_exn   "keepass_db_password_required" values;
          keepass_db_keyfile           = assoc_string_opt_exn "keepass_db_keyfile" values; }
      with
      | Failure e -> Result.Error e
      end
  | _ ->
      Result.Error "app_config parse error: invalid format json."
  | exception (Yojson.Json_error msg) ->
      Result.Error ("app_config parse error: " ^ msg)
