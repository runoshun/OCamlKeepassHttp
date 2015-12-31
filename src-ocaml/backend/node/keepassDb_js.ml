open Js

class type js_keepass_db = object
end

class type js_keepass_string = object
  method _Key   : js_string t prop
  method _Value_str : js_string t prop
  method _Value_obj : 'a prop
end

(* XXX_arr and XXX_obj:
 *  These are for limitation of xml2js.
 *  Even if, we serialize array which have single element,
 *  it will be deserialize as the element itself. *)
class type js_keepass_entry = object
  method _UUID  : js_string t prop
  method _String_arr : js_keepass_string t js_array t writeonly_prop
  method _String_obj : js_keepass_string t optdef_prop
end

class type js_keepass_node = object
  method _UUID  : js_string t prop
  method _Name  : js_string t prop
  method _Entry_arr : js_keepass_entry t js_array t writeonly_prop
  method _Entry_obj : js_keepass_entry t optdef_prop
  method _Group_arr : js_keepass_node t js_array t writeonly_prop
  method _Group_obj : js_keepass_node t optdef_prop
end

class type js_keepass_utils = object
  method openDb : js_string t -> js_string t opt -> js_string t opt
                  -> (js_keepass_db t -> error t opt -> unit) callback
                  -> unit meth
  method saveDb : js_keepass_db t -> js_string t
                  -> (js_keepass_db t -> error t opt -> unit) callback
                  -> unit meth
  method getRootGroup : js_keepass_db t -> js_keepass_node t meth
  method setRootGroup : js_keepass_db t -> js_keepass_node t -> unit meth

  method createGroup : js_string t -> js_string t -> js_keepass_node t meth
  method createStringEntity : js_string t -> js_string t -> bool t -> js_keepass_string t meth
  method createEntry : js_string t -> js_keepass_string t js_array t -> js_keepass_entry t meth
end

let keepass_utils : js_keepass_utils t = Js.Unsafe.eval_string "
(function() {
  var kpio = require('keepass.io');

  var makeDbWithCredentials = function(password, keyfile) {
      var db = new kpio.Database();
      if(password) {
          db.addCredential(new kpio.Credentials.Password(password));
      }
      if(keyfile) {
          db.addCredential(new kpio.Credentials.Keyfile(keyfile));
      }
      return db;
  };

  return {

      openDb : function(kdbx, password, keyfile, callback) {
          if(!keyfile) { keyfile = null; }
          var db = makeDbWithCredentials(password,keyfile);
          db.loadFile(kdbx, function(err) { callback(db,err); });
      },

      saveDb : function(db, kdbx, callback) {
          db.saveFile(kdbx, function(err) { callback(db,err); });
      },

      getRootGroup : function(db) {
          return db.getRawApi().get().KeePassFile.Root.Group;
      },

      setRootGroup : function(db, rootGroup) {
          var rawDatabase = db.getRawApi().get();
          rawDatabase.KeePassFile.Root.Group = rootGroup;
          db.getRawApi().set(rawDatabase);
      },

      createGroup : function(name,uuid) {
          return {
              'Name' : name,
              'UUID' : uuid,
          };
      },

      createStringEntity : function(key,value,is_protected) {
          if(is_protected) {
              return {
                  'Key' : key,
                  'Value' : {
                      '$' : { 'Protected' : 'True' },
                      '_' : value
                  }
              }
          }
          else {
              return {
                  'Key' : key,
                  'Value' : value
              };
          }
      },

      createEntry : function(uuid, string) {
          return {
              'UUID' : uuid,
              'String' : string };
      }

  };
})(); "

type t = js_keepass_db Js.t

type keepass_entry = {
  kp_uuid     : string;
  kp_title    : string option;
  kp_username : string option;
  kp_password : string option;
  kp_url      : string option;
}

let to_js_credentials password keyfile =
  let pass = match password with
  | Some p -> Some (Js.string p)
  | None   -> None in
  let keyf = match keyfile with
  | Some k -> Some (Js.string k)
  | None   -> None in
  (pass,keyf)

let to_js_callback cb =
  Js.wrap_callback
    (fun db js_err ->
      let err = Opt.case js_err
          (fun () -> None)
          (fun e -> Some(Js.string_of_error e)) in
      cb db err)

let open_db db_path ?(password=None) ?(keyfile=None) cb =
  match to_js_credentials password keyfile with
  | (None, None) -> raise (Failure "Must be not empty both password and keyfile.")
  | (pass, keyf) ->
    keepass_utils##openDb
      (Js.string db_path)
      (Js.Opt.option pass)
      (Js.Opt.option keyf)
      (to_js_callback cb)

let save_db db db_path cb =
  keepass_utils##saveDb
    db
    (Js.string db_path)
    (to_js_callback cb)

let as_js_array obj : 'a js_array Js.t =
  Js.Optdef.case obj
    (fun () -> Js.array [| |])
    (fun obj ->
       if NodeUtils.is_array obj then
         Js.Unsafe.coerce obj
       else
         Js.array [| obj |])

let ensure_entry (node : js_keepass_node Js.t) =
  let js_entry = as_js_array (node##._Entry_obj) in
  node##._Entry_arr := js_entry;
  js_entry

let ensure_group (node : js_keepass_node Js.t) =
  let js_group = as_js_array (node##._Group_obj) in
  node##._Group_arr := js_group;
  js_group

let ensure_string (entry : js_keepass_entry Js.t) =
  let js_strings = as_js_array (entry##._String_obj) in
  entry##._String_arr := js_strings;
  js_strings

let find_group_by_name name (groups : js_keepass_node Js.t js_array Js.t) =
  Array.fold_left
    (fun r g -> if (Js.to_string g##._Name) = name then Some g else r)
    None (Js.to_array groups)

let find_group root_group path =
  List.fold_left (fun (node:js_keepass_node Js.t option) name ->
    match node with
    | Some node -> find_group_by_name name (as_js_array (node##._Group_obj))
    | None -> None)
    (Some root_group) path

let find_or_create_group root_group path =
  List.fold_left (fun (node:js_keepass_node Js.t) name ->
    match (find_group_by_name name (as_js_array (node##._Group_obj))) with
    | Some g -> g
    | None ->
        let uuid_b64 = Uuid.to_hex_base64 (Uuid.gen_v4 ()) in
        let new_group = keepass_utils##createGroup (Js.string name) (Js.string (Base64.to_string uuid_b64)) in
        let group_arr = ensure_group node in
        group_arr##push new_group |> ignore;
        new_group)
  root_group path

let get_entries ?(path=[]) db =
  let build_entry uuid (js_entries : js_keepass_string Js.t js_array Js.t) =
    let module StringMap = Map.Make(String) in
    let entry_map = Array.fold_left (fun m entry ->
        StringMap.add (Js.to_string entry##._Key) entry m)
      StringMap.empty
      (Js.to_array js_entries) in
    let find_value map key if_found =
      try if_found (StringMap.find key map) with
      | Not_found -> None
    in
    let as_string = (fun item -> Some (Js.to_string item##._Value_str)) in
    let kp_uuid     = (Js.to_string uuid) in
    let kp_title    = find_value entry_map "Title"    as_string in
    let kp_username = find_value entry_map "UserName" as_string in
    let kp_url      = find_value entry_map "URL"      as_string in
    let kp_password = find_value entry_map "Password"
      (fun item ->
        match (Js.to_string (Js.typeof item##._Value_obj)) with
        | "object" -> Some (Js.to_string (Js.Unsafe.get (item##._Value_obj) (Js.string "_")))
        | "string" -> Some (Js.to_string (item##._Value_str))
        | _ -> None) in
    { kp_uuid; kp_title; kp_username; kp_url; kp_password }
  in
  let rec find_strings (group : js_keepass_node Js.t) =
    let js_entries_arr = as_js_array (group##._Entry_obj) in
    let entries = Array.map (fun e -> build_entry (e##._UUID) (as_js_array (e##._String_obj)))
                            (Js.to_array js_entries_arr) in
    let js_groups = as_js_array (group##._Group_obj) in
    let arr = Array.map find_strings (Js.to_array js_groups) in
    Array.append entries (Array.concat (Array.to_list arr))
  in
  let root_group = keepass_utils##getRootGroup (db) in
  match find_group root_group path with
  | Some g -> find_strings g
  | None -> [| |]

let create_entry db path entry =
  let root_group = keepass_utils##getRootGroup (db) in
  let target_group = find_or_create_group root_group path in
  let js_entry_arr = ensure_entry target_group in
  let strings = List.fold_left (fun lst (k, v, is_protected) ->
    match v with
    | Some v ->
        let new_entry_val = keepass_utils##createStringEntity
            (Js.string k) (Js.string v) (Js.bool is_protected) in
        new_entry_val :: lst
    | None -> lst)
    []
    [("Title", entry.kp_title, false);
     ("UserName", entry.kp_username, false);
     ("URL", entry.kp_url, false);
     ("Password", entry.kp_password, true);] in
  let entry = keepass_utils##createEntry
    (Js.string entry.kp_uuid) (Js.array (Array.of_list strings)) in
  js_entry_arr##push (entry) |> ignore;
  keepass_utils##setRootGroup db root_group

let dump_db db =
  let root_group = keepass_utils##getRootGroup (db) in
  let json = Js.Unsafe.js_expr "JSON" in
  Js.to_string (json##stringify (root_group,Js.null,Js.number_of_float 2.0))

