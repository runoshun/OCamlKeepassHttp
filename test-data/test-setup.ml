#use "topfind"
#require "fileutils"

let () =
  FileUtil.rm ["test-data/create_and_save.kdbx"; "test-data/saved.kdbx"];
  FileUtil.cp ["test-data/both.kdbx"] "test-data/create_and_save.kdbx";
  FileUtil.cp ["test-data/pass.kdbx"] "test-data/provider.kdbx"
