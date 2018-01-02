type action_type =
  | ActTypeAssociate of string

type action = {
  act_id   : Uuid.t;
  act_type : action_type;
  act_date : string;
  act_perform : Yojson.Safe.json -> (string MyResult.t -> unit) -> unit;
}

type 'a app_state = {
  config       : AppConfig.t ref;
  config_path  : string ref;
  server       : 'a option ref;
  password     : string option ref;
  actions      : action list ref;
}

type request =
  | ReqGetStatic of string
  | ReqGetState
  | ReqPostConfig of string
  | ReqPostAction of string
  | ReqPostDeleteAction of string
  | ReqPostRestartKeepassHttpServer
  | ReqInvalid

type 'a response =
  | ResGetStatic of string
  | ResGetState of 'a app_state
  | ResSuccess
  | ResError of string
  | ResInvalid of string
  | ResNotFound
