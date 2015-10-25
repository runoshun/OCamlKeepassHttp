OCamlKeepassHttp
================

Keepass HTTP compatible server running on nodejs, written by ocaml.

## Build Requirements
OCaml version >= 4.02.1

Following OCaml libraries are required for build.
- js\_of\_ocaml
- base64
- yojson
- ppx\_deriving

## Runtime Requirements
nodejs >= 0.10.32

Following nodejs libraries are required for runtime.
- keepass.io (GPLv3)

## Build and Run
    make
    node node_okhd.js

## Configuration
node\_okhd.js read a configuration file specified by -f option or '~/.okhd-conf'.
Example of configuration file is
    { httpserver_host : "127.0.0.1",              /* server's hostname (default: "127.0.0.1") */
      httpserver_port : 19456,                    /* port of main http server (default: 19455) */
      keepass_db : "/home/shun/tmp/keepass.kdbx", /* [required] path to keepass db file (supported only *.kdbx file) */
      keepass_db_password_required : true,        /* if true, password prompt is show on launching (default: false) */
      keepass_db_keyfile : "/home/shun/k.key" }   /* path to key file of keepass db (default: none) */

