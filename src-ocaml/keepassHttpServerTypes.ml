type request_auth = {
  req_id        : string;
  req_nonce     : Base64.t;
  req_verifier  : Base64.t;
}
[@@deriving show]

type request_associate = {
  req_key : Base64.t;
}
[@@deriving show]

type request_get_logins = {
  req_url : Base64.t;
  (* TODO : add submit_url *)
}
[@@deriving show]

type request_set_login = {
  req_submit_url : Base64.t;
  req_login      : Base64.t;
  req_password   : Base64.t;
}
[@@deriving show]

type request =
  | ReqTestAssociate  of request_auth
  | ReqTestAssociateCheck
  | ReqAssociate      of request_associate
  | ReqGetLogins      of request_auth * request_get_logins
  | ReqGetLoginsCount of request_auth * request_get_logins
  | ReqSetLogin       of request_auth * request_set_login
  | ReqGeneratePassword of request_auth
  | ReqInvalid
[@@deriving show]

type login_entry = {
  entry_name     : Base64.t;
  entry_login    : Base64.t;
  entry_password : Base64.t;
  entry_uuid     : Base64.t;
}
[@@deriving show]

type response_common = {
  res_success : bool;
  res_nonce   : Base64.t;
  res_verifier: Base64.t;
}
[@@deriving show]

type response_test_associate = {
  res_id : string;
}
[@@deriving show]

type response_associate = {
  res_id : string;
}
[@@deriving show]

type response_get_logins = {
  res_id : string;
  res_entries : login_entry list;
}
[@@deriving show]

type response_get_login_count = {
  res_count : int;
}
[@@deriving show]

type response =
  | ResTestAssociate  of response_common * response_test_associate
  | ResTestAssociateCheck
  | ResAssociate      of response_common * response_associate
  | ResGetLogins      of response_common * response_get_logins
  | ResSetLogin       of response_common
  | ResGetLoginsCount of response_common * response_get_login_count
  | ResGeneratePassword of response_common * response_get_logins
  | ResFailed
[@@deriving show]

