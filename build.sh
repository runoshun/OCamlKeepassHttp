 #!/bin/sh
ocamlbuild -use-ocamlfind -build-dir _build  -Xs "node_modules,semantic,bin,js,test-data" src-ocaml/index.byte
js_of_ocaml --opt 3  -o index.js _build/src-ocaml/index.byte
