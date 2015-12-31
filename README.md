OCamlKeepassHttp
================

Keepass HTTP compatible server running on nodejs, written by ocaml.

## Using
### Install and Run
You can install via npm.

    npm install -g node-keepass-http
    node-keepass-http

After launching, configuration screen is opened in browser automatically. 

### Runtime Requirements
nodejs >= 0.10.32

Following nodejs libraries are required at runtime.

 - keepass.io (GPLv3)
 - open

### Configuration
node-keepass-http read a configuration file specified by -c option or '~/.node-keepass-http.conf' in default. Example of configuration file is

    {   "configserver_host": "localhost", /* host of config screen server */
        "configserver_port": 18080        /* port of config screen server */ }

Other fields are generated automatically when config changed. 

# Building
## Build Requirements
OCaml version >= 4.02.1

Following OCaml libraries are required for build.

- js\_of\_ocaml
- base64
- yojson
- ppx\_deriving

## Build and Run
    sh ./build.sh
    node -e "require('./index.js').start()"

