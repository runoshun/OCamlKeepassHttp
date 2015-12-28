(define-default-config
  (exclude-dirs node_modules semantic bin js test-data))

(define-default-component "indexjs")

(define-exe "indexjs"
  (target-name index)
  (test-name test_all)

  (ocaml-packages js_of_ocaml base64 yojson ppx_deriving.std)
  (ocaml-test-packages oUnit)

  (main-target release-js)
  (check-target mycheck)

  (src-dir src-ocaml)
  (test-dir test-ocaml)

  (test-js-prim test-ocaml/prim_unix.js)

  (tags-additionals (c < $src-dir "/backend/node/*.{ml,mli}> : syntax(camlp4o), package(js_of_ocaml.syntax)")
                    (c < $src-dir / $target-name ".ml> : syntax(camlp4o), package(js_of_ocaml.syntax)")
                    (c < $test-dir "/node/*.{ml,mli}> : syntax(camlp4o), package(js_of_ocaml.syntax)")))

(gmk-eval "
mycheck:
	ocaml test-data/test-setup.ml
	$(MAKE) C=$(C) check-js
")
