open OUnit

let test_get_hostname1 () =
  begin match (UrlUtil_js.get_host_name "http://www.example.com") with
  | Some hostname -> OUnit.assert_equal "www.example.com" hostname
  | None          -> OUnit.assert_failure "can't parse url"
  end

let test_get_hostname2 () =
  begin match (UrlUtil_js.get_host_name "http://www.example.com/path/to/content") with
  | Some hostname -> OUnit.assert_equal "www.example.com" hostname
  | None          -> OUnit.assert_failure "can't parse url"
  end

let test_get_invalid_hostname () =
  begin match (UrlUtil_js.get_host_name "foo.bar") with
  | Some hostname -> OUnit.assert_failure "illegal data passed, but parsed successflly."
  | None          -> OUnit.assert_bool "_" true
  end

let tests = "UrlUtil_js" >::: [
  TestCase test_get_hostname1;
  TestCase test_get_hostname2;
  TestCase test_get_invalid_hostname;
]
