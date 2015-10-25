
type logger = {
  debug : string -> unit;
  info  : string -> unit;
  error : string -> unit;
  log_level : int ref;
}

let empty_logger = {
  debug = (fun _ -> ());
  info  = (fun _ -> ());
  error = (fun _ -> ());
  log_level = ref 2;
}

let logger : logger ref = ref empty_logger

let init backend =
  let module Backend = (val backend : Backends.Interface) in
  let log_level = ref 2 in
  let print = Backend.Sys.print in
  logger := {
    debug = (fun msg -> if !log_level > 2 then print (Printf.sprintf "[debug] %s\n" msg));
    info  = (fun msg -> if !log_level > 1 then print (Printf.sprintf "[info]  %s\n" msg));
    error = (fun msg -> if !log_level > 0 then print (Printf.sprintf "[error] %s\n" msg));
    log_level = log_level;
  }

let debug msg = !logger.debug msg
let info  msg = !logger.info msg
let error msg = !logger.error msg
let set_log_level level = !logger.log_level := level
