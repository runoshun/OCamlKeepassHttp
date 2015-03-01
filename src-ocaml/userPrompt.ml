
module type UserPromptInterface = sig

  type prompt_options = {
    message : string;
    input_password : bool;
  }

  val prompt : ?options:prompt_options -> (string Result.t -> unit) -> unit

end

module MakeHttpPrompt(Backends : Backends.Interface) : UserPromptInterface = struct

  module HttpServer = Backends.HttpServer

  type prompt_options = {
    message : string;
    input_password : bool;
  }

  let make_handle_request opts callback = (fun server req send_res ->
    let open HttpServer in
    match req.hr_method with
    | "GET" ->
        let input_type = if opts.input_password then "password" else "text" in
        (* show msg as html*)
        send_res "text/html" (Printf.sprintf "
        <body>
          <p>%s</p>
          <form method=\"POST\" action=\"/\">
            <input name=\"value\" type=\"%s\" />
            <input type=\"submit\" />
          </form>
        </body>
        " opts.message input_type)
    | "POST" ->
        (* handle submited data from html form *)
        let body = req.hr_body in
        Logger.debug ("body : " ^ body);
        let start_idx = (String.index body '=') + 1 in
        begin try
          if String.sub body 0 (start_idx - 1) = "value" then begin
            let value = String.sub body start_idx (String.length body - start_idx) in
            callback (Result.Ok value);
            send_res "text/plain" ("successfully submited.")
          end else
            raise (Invalid_argument "key is not 'value'")
        with
        | Invalid_argument s ->
            callback (Result.Error ("can't parse value from posted data: " ^ s));
            send_res "text/plain" "can't submit value, parse error occured.";
        end;
        HttpServer.stop_server server
    | m ->
        callback (Result.Error ("unknown request method :" ^ m));
        HttpServer.stop_server server)

  let port_counter = ref 18080
  let get_port () =
    port_counter := !port_counter + 1;
    !port_counter

  let prompt ?(options={ message = "input value"; input_password = false}) callback =
    let handler = make_handle_request options callback in
    let server = HttpServer.create_server handler in
    let port = get_port () in
    HttpServer.start_server server ~port ~host:"localhost" ~callback:(fun _ ->
      let url = Printf.sprintf "http://localhost:%d" port in
      Logger.info (Printf.sprintf "User input is required. Please confirm on %s." url);
      Backends.Sys.open_app url
    );

end
