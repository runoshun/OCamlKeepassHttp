open OUnit

let no_conflict n =
  let uuids = Array.to_list (Array.init n (fun _ -> Uuid.gen_v4 ())) in
  List.iter (fun uuid ->
    let same_uuids = List.find_all (fun uuid2 -> uuid = uuid2) uuids in
    assert_bool "generated uuid is conflict" (List.length same_uuids = 1))
    uuids

let uuid_length () =
  let uuids = Array.to_list (Array.init 10 (fun _ -> Uuid.gen_v4 ())) in
  assert_bool "uuid length is invalid"
   (List.for_all (fun uuid -> (String.length uuid = 36)) uuids)

(* uuid v4 header is 1001 00xx xxxx ... *)
let uuid_v4_header () =
  let uuids = Array.to_list (Array.init 10 (fun _ -> Uuid.gen_v4 ())) in
  assert_bool "uuid v4 header is invalid"
    (List.for_all (fun uuid -> (uuid.[0] == '9' &&
                               (uuid.[1] == '0' ||
                                uuid.[1] == '1' ||
                                uuid.[1] == '2' ||
                                uuid.[1] == '3'))) uuids)

let tests = "uuid" >::: [
  TestCase (fun () -> no_conflict 10);
  TestCase (fun () -> no_conflict 1000);
  TestCase uuid_length;
  TestCase uuid_v4_header;
]
