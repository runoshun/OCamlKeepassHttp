open OUnit
open KeepassDb_js

let pass_db_path    = "test-data/pass.kdbx"
let keyfile_db_path = "test-data/keyfile.kdbx"
let both_db_path    = "test-data/both.kdbx"
let saved_db_path   = "test-data/saved.kdbx"
let create_and_save_db_path   = "test-data/create_and_save.kdbx"

let password = Some "test"
let keyfile = Some "test-data/keyfile.key"

let pass_db     = ref (MyResult.Error "db is not set.")
let keyfile_db  = ref (MyResult.Error "db is not set.")
let both_db     = ref (MyResult.Error "db is not set.")
let saved_db    = ref (MyResult.Error "db is not set.")
let create_and_save_db = ref (MyResult.Error "db is not set.")

let set_db ref db err =
  match err with
  | Some e -> ref := MyResult.Error e
  | None   -> ref := MyResult.Ok db

let test_open_password_db () =
  match !pass_db with
  | MyResult.Error s -> assert_failure s
  | MyResult.Ok db -> ()

let test_open_keyfile_db () =
  match !keyfile_db with
  | MyResult.Error s -> assert_failure s
  | MyResult.Ok db -> ()

let test_open_both_db () =
  match !both_db with
  | MyResult.Error s -> assert_failure s
  | MyResult.Ok db -> ()

let test_get_entries_all () =
  match !pass_db with
  | MyResult.Error s -> assert_failure s
  | MyResult.Ok db ->
      let entries = KeepassDb_js.get_entries db in
      let printer = function Some s -> s | None -> "None" in
      assert_equal 2 (Array.length entries);
      Array.iter (fun entry ->
        begin match entry.kp_title with
        | None -> assert_failure "title not found"
        | Some title ->
            begin match title with
            | "Sample Entry" ->
              assert_equal ~printer (Some "http://keepass.info/") entry.kp_url;
              assert_equal ~printer (Some "User Name") entry.kp_username;
              assert_equal ~printer (Some "Password") entry.kp_password
            | "Sample Entry #2" ->
              assert_equal ~printer (Some "http://keepass.info/help/kb/testform.html") entry.kp_url;
              assert_equal ~printer (Some "Michael321") entry.kp_username;
              assert_equal ~printer (Some "12345") entry.kp_password
            | _ ->
              assert_failure "Unknown entry title."
            end
        end)
      entries

let test_create_entry () =
  match !keyfile_db with
  | MyResult.Error s -> assert_failure s
  | MyResult.Ok db ->
      let uuid = Uuid.gen_v4 () in
      let printer = function Some s -> s | None -> "None" in
      KeepassDb_js.create_entry db ["TestGroup"; "TestEntries"]
        { kp_uuid  = Uuid.to_string uuid;
          kp_title = Some "Test Entry";
          kp_url   = Some "http://www.example.com";
          kp_username = Some "Test User";
          kp_password = Some "TestPassword"; };
      let entries = KeepassDb_js.get_entries db ~path:["TestGroup"; "TestEntries"] in
      assert_equal 1 (Array.length entries);
      Array.iter (fun entry ->
        begin match entry.kp_title with
        | None -> assert_failure "title not found"
        | Some title ->
            begin match title with
            | "Test Entry" ->
                assert_equal (Uuid.to_string uuid) entry.kp_uuid;
                assert_equal ~printer (Some "http://www.example.com") entry.kp_url;
                assert_equal ~printer (Some "Test User") entry.kp_username;
                assert_equal ~printer (Some "TestPassword") entry.kp_password;
            | _ -> assert_failure "Unknown entry title."
            end
        end) entries

let setup_save_db_test thunk =
  begin match !both_db with
  | MyResult.Error m -> assert_failure ("setup_save_db is failed." ^ m)
  | MyResult.Ok db ->
      KeepassDb_js.save_db db saved_db_path (fun db err ->
        begin match err with
        | Some m -> assert_failure ("setup_save_db is failed." ^ m)
        | None   ->
            KeepassDb_js.open_db saved_db_path ~password ~keyfile (fun db err ->
              set_db saved_db db err;
              thunk ())
        end)
  end

let test_save_db () =
  match !saved_db with
  | MyResult.Error m -> assert_failure m
  | MyResult.Ok db ->
      let entries = KeepassDb_js.get_entries db in
      assert_equal 2 (Array.length entries)

let setup_create_and_savedb thunk =
  KeepassDb_js.open_db create_and_save_db_path ~password ~keyfile (fun db err ->
      set_db create_and_save_db db err;
      (* create entry *)
      begin match !create_and_save_db with
      | MyResult.Error m -> assert_failure ("setup_create_and_savedb is failed. " ^ m)
      | MyResult.Ok db ->
          KeepassDb_js.create_entry db ["***TestGroup***"; "******TestEntries*****"]
            { kp_uuid  = (Uuid.to_string (Uuid.gen_v4 ()));
              kp_title = Some "Test Entry";
              kp_url   = Some "http://www.example.com";
              kp_username = Some "Test User";
              kp_password = Some "TestPassword"; };
          (* save db and reopen db *)
          KeepassDb_js.save_db db create_and_save_db_path (fun db err ->
              KeepassDb_js.open_db create_and_save_db_path ~password ~keyfile (fun db err ->
                  set_db create_and_save_db db err;
                  thunk ()))
      end)

let test_create_and_savedb () =
  match !create_and_save_db with
  | MyResult.Error m -> assert_failure ("test_create_and_savedb failed. " ^ m)
  | MyResult.Ok db ->
      let entries = KeepassDb_js.get_entries db in
      let printer = function Some s -> s | None -> "None" in
      assert_equal ~printer:string_of_int 3 (Array.length entries);
      Array.iter (fun entry ->
        begin match entry.kp_title with
        | None -> assert_failure "title not found"
        | Some title ->
            begin match title with
            | "Test Entry" ->
                assert_equal ~printer (Some "http://www.example.com") entry.kp_url;
                assert_equal ~printer (Some "Test User") entry.kp_username;
                assert_equal ~printer (Some "TestPassword") entry.kp_password;
            | "Sample Entry" | "Sample Entry #2" -> ()
            | t -> assert_failure ("Unknown entry title." ^ t)
            end
        end) entries



let init thunk =
  KeepassDb_js.open_db pass_db_path ~password (fun db err ->
      set_db pass_db db err;
      KeepassDb_js.open_db keyfile_db_path ~keyfile (fun db err ->
          set_db keyfile_db db err;
          KeepassDb_js.open_db both_db_path ~password ~keyfile (fun db err ->
              set_db both_db db err;
              setup_save_db_test (fun () ->
                  setup_create_and_savedb (fun () ->
                      thunk ())
                  ))))

let tests = "js_keepass_db" >::: [
  TestCase test_open_password_db;
  TestCase test_open_keyfile_db;
  TestCase test_open_both_db;
  TestCase test_get_entries_all;
  TestCase test_create_entry;
  TestCase test_save_db;
  TestCase test_create_and_savedb;
]
