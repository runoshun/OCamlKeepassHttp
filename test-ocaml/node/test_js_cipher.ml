open OUnit

let cipher = Cipher_js.create_cipher Cipher_js.AES_CBC (Base64.encode "1234567890123456") (Base64.encode "0000000000000000")

let assert_encrypt data encrypted =
  match cipher with
  | MyResult.Ok cipher -> 
      let encrypted_msg = Cipher_js.encrypt cipher data in
      assert_equal ~printer:(fun x->x) encrypted (Base64.to_string encrypted_msg)
  | MyResult.Error msg ->
      assert_failure msg

let assert_decrypt data encrypted =
  match cipher with
  | MyResult.Ok cipher -> 
      let msg = Cipher_js.decrypt cipher (Base64.of_string encrypted) in
      assert_equal ~printer:(fun x->x) data msg
  | MyResult.Error msg ->
      assert_failure msg

let encrypt_data1_test () =
  assert_encrypt
    "hello world 1234"
    "4ZetrfLZAe9BQYWjESe6B9buOB2pVJdo4HEmgEa6BkA="

let decrypt_data1_test () =
  assert_encrypt
    "hello world 1234"
    "4ZetrfLZAe9BQYWjESe6B9buOB2pVJdo4HEmgEa6BkA="

let encrypt_data2_test () =
  assert_decrypt
    "hello world"
    "NuYHK/gWopkFB5VUf8bvfw=="

let decrypt_data2_test () =
  assert_decrypt
    "hello world"
    "NuYHK/gWopkFB5VUf8bvfw=="

let tests = "Cipher_js" >::: [
  TestCase encrypt_data1_test;
  TestCase decrypt_data1_test;
  TestCase encrypt_data2_test;
  TestCase decrypt_data2_test;
]
