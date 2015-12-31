(define-default-config
  (main-targets index-js-release)
  (check-targets mycheck)
  (exclude-dirs node_modules semantic bin js test-data))

(define-default-component "index")

(define-exe "index"
  (target-name index)
  (test-name test_all)
  (enable-formats byte js)

  (src-dir src-ocaml)
  (test-dir test-ocaml)

  (ocaml-packages js_of_ocaml js_of_ocaml.ppx base64 yojson ppx_deriving.std)
  (ocaml-test-packages oUnit)

  (test-js-primitives test-ocaml/prim_unix.js))

(gmk-eval "
mycheck:
	ocaml test-data/test-setup.ml
	$(MAKE) C=$(C) index-check-js
")
