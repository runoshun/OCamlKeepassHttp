OCamlKeepassHttp
================

Keepass HTTP compatible server running on nodejs, written by ocaml.

## Install and Run
    npm install -g node-keepass-http
    node-keepass-http

## Runtime Requirements
nodejs >= 0.10.32

Following nodejs libraries are required at runtime.
 - keepass.io (GPLv3)

## Build Requirements
OCaml version >= 4.02.1

Following OCaml libraries are required for build.
- js\_of\_ocaml
- base64
- yojson
- ppx\_deriving

## Build and Run
    make
    node node_okhd.js

## Configuration
node\_okhd.js read a configuration file specified by -f option or '~/.node-keepass-http.conf'.
Example of configuration file is

    {   "configserver_host": "localhost", /* host of config screen server */
        "configserver_port": 18080        /* port of config screen server */ }

Other fields are generated automatically when config changed. 
