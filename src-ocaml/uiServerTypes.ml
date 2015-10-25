type 'a app_state = {
  config      : AppConfig.t ref;
  config_path : string ref;
  server      : 'a option ref;
  password    : string option ref;
}

type request =
  | ReqGetStatic of string
  | ReqGetState
  | ReqPostConfig of string
  | ReqPostRestartKeepassHttpServer
  | ReqInvalid

type 'a response =
  | ResGetStatic of string
  | ResGetState of 'a app_state
  | ResSuccess
  | ResError of string
  | ResInvalid of string
  | ResNotFound
