#;@@scheme
define guile_lib

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 pretty-print)
             (ice-9 match))

(define sp #\space)
(define empty-string "")
(define (string-empty? s) (string=? empty-string s))

(define (str o)
  (cond ((symbol? o) (symbol->string o))
        ((string? o) o)
        ((number? o) (number->string o))))

;;;;;;;;;;;;;;;;;;;;;;;;;;  Common Functions  ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (filename-join #:rest args)
  (string-join args file-name-separator-string))

(use-modules (ice-9 ftw))
(define (find-sources dir exts-str)
  (define exts (string-split exts-str sp))
  (define (select? name)
    (let ((idx (string-rindex name #\.)))
      (member (substring name (if idx idx 0)) exts)))
  (map (lambda (f) (filename-join dir f)) (scandir dir select?)))

(define (find-subdirs dir)
  (define (enter? name stat result)
    (not (char=? #\. (string-ref name 0))))
  (define (none name stat result) result)
  (define (down name stat result) (cons name result))
  (define (error name stat errno result)
    (format (current-error-port) "warning: ~a: errno:~a~%" name errno))
  (let ((r (file-system-fold enter? none down none none error '() dir)))
    (if (list? r) r '())))

(define (get-rel-prj-root basedir)
  (let ((paths (string-split basedir (string-ref file-name-separator-string 0))))
    (apply filename-join (map (lambda (_) "..") paths))))

(define (alist-update key val alist)
  (alist-cons key val (alist-delete key alist)))

(define (alist-append-value key val alist)
  (let* ((old-val (assoc key alist))
         (new-val (if old-val (append (cdr old-val) val) val)))
    (alist-update key new-val alist)))

(define* (alist-assoc-value key alist #:optional (default '()))
  (let ((r (assoc key alist)))
    (if r (cdr r) default)))

(define (tsort nodes links)
  (define (out-links n links)
    (fold (lambda (l ls) (if (equal? (car l) n) (cons l ls) ls)) '() links))
  (define (in-links n links)
    (fold (lambda (l ls) (if (equal? (cdr l) n) (cons l ls) ls)) '() links))
  (let loop ((sorted '())
             (links links)
             (s (remove (cut member <> (map cdr links)) nodes)))
    (if (null? s)
      (if (null? links) (reverse sorted) #f)
      (let* ((sorted (cons (car s) sorted))
             (outs (out-links (car s) links))
             (links (remove (cut member <> outs) links))
             (update-s (lambda (out s) (if (null? (in-links (cdr out) links))
                                         (cons (cdr out) s)
                                         s))))
        (loop sorted links (fold update-s (cdr s) outs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;  Functions for project.scm  ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define COMPONENTS '())
(define EXPAND_RULES '())
(define DEFAULT_CONFIG '())
(define DEFAULT_COMPONENT "main")

(define (add-component comp)
  (set! COMPONENTS (cons comp COMPONENTS)))

(define-syntax define-default-config
  (syntax-rules ()
    ((_ . conf)
     (set! DEFAULT_CONFIG 'conf))))

(define (define-default-component component)
  (set! DEFAULT_COMPONENT component))

(define-syntax make-define-component-type
  (syntax-rules ()
    ((_ name defaults)
     (define-syntax name
       (syntax-rules ()
         ((_ comp . conf)
          (let ((config (expand-configs (append (quote conf) DEFAULT_CONFIG))))
            (add-component (cons comp (append config (quote defaults)))))))))))

(define (expand-configs configs)
  (fold (lambda (config configs)
          (fold (lambda (rule configs)
                  (if ((car rule) (car config) (cdr config) configs)
                    ((cdr rule) (car config) (cdr config) configs COMPONENTS)
                    configs))
                configs
                EXPAND_RULES))
        configs
        configs))

(define-syntax define-expand-rule
  (syntax-rules ()
    ((_ pred expand)
     (set! EXPAND_RULES (cons (cons pred expand) EXPAND_RULES)))))

(define (merge-configs configs new-configs)
  (fold (lambda (config configs) (alist-append-value (car config) (cdr config) configs))
        configs
        new-configs))

(define-expand-rule ; +config
  (lambda (n v configs)
    (and (char=? #\+ (string-ref (str n) 0))
         (> (string-length (str n)) 2)))
  (lambda (n v configs comps)
    (let* ((c-name (string->symbol (substring (str n) 1))))
      (alist-append-value c-name v configs))))

(define (check-config)
  (define (error msg . args) (gmk-eval (format #f "$$(error Config Error: ~a)~%" (apply format #f msg args))))
  (if (null? COMPONENTS)
    (error "no component is defined."))
  (if (not (assoc (get-current-component) COMPONENTS))
    (error "component '~a' is not defined." (get-current-component))))

;;;;;;;;;;;;;;;;;;;;;;;;;;  Functions for Configs  ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (config-> configs x unit return join)
  (define to-> (lambda (x) (config-> configs x unit return join)))
  (define (ref-config sym)
    (let ((r (assoc sym configs)))
      (if r (to-> (cdr r)) unit)))
  (cond
    ((boolean? x) (if x "true" "false"))
    ((string? x) (return x))
    ((and (symbol? x) (equal? #\$$ (string-ref (symbol->string x) 0)))
     (ref-config (string->symbol (substring (symbol->string x) 1))))
    ((symbol? x)
     (return (symbol->string x)))
    ((number? x)
     (return (number->string x)))
    ((null? x)
     x)
    ((and (list? x) (equal? (car x) 'c))
     (return (string-join (join (map to-> (cdr x))) empty-string)))
    ((and (list? x) (equal? (car x) 'e))
     (to-> (eval (cadr x) (interaction-environment))))
    ((list? x)
     (join (map to-> x)))))

(define (config-to-string configs x)
  (config-> configs x empty-string (lambda (x) x) string-join))

(define (config-to-list configs x)
  (config-> configs x '() list (lambda (args) (apply append args))))

(define (make->scm s)
  (list->string
    (map (lambda (x) (cond ((char=? #\_ x) #\-)
                           (#t (char-downcase x))))
         (string->list s))))

(define (get-component-configs name)
  (let ((r (assoc name COMPONENTS)))
    (if r (cdr r) '())))

(define* (get-config component name #:optional (to-list? #f))
  (define conv-config (if to-list? config-to-list config-to-string))
  (define configs (get-component-configs component))
  (let ((r (assoc (string->symbol (make->scm name)) configs)))
    (if r (conv-config configs (cdr r)) r)))

(define* (get-var component name #:optional (to-list? #f) #:key (default (if to-list? '() "")))
  (let ((c (get-config component name to-list?))
        (v (gmk-var name)))
    (cond
      ((and c (not to-list?)) c)
      ((and c to-list?) (remove string-empty? c))
      ((and (not c) (string-empty? v)) default)
      ((and (not c) to-list?) (remove string-empty? (string-split v sp)))
      ((and (not c) (not to-list?)) v))))

(define (get-components)
  (let* ((comps (map car COMPONENTS))
         (links (apply append (map (lambda (c) (map (cut cons c <>) (get-var c "DEPEND_COMPONENTS" #t))) comps))))
    (reverse (tsort comps links))))

(define (get-current-component)
  (let ((c (gmk-expand "$(C)")))
    (if (string-empty? c) DEFAULT_COMPONENT c)))

(define* (define-makefile-vars vars #:optional (c (get-current-component)) (pred (lambda (x) x)))
  (for-each (lambda (v)
              (let ((val (get-var c (symbol->string v))))
                (if (pred val) (gmk-eval (format #f "~a := ~a~%" v val)))))
            vars))

;;;;;;;;;;;;;;;;;;;;;;;;;;  Functions for OCaml project.scm  ;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-define-component-type define-exe
   ((comp-type program)
    (bin-type exe)
    (enable-formats byte native)))

(make-define-component-type define-ppx
   ((comp-type ppx)
    (bin-type exe)
    (enable-formats byte)))

(make-define-component-type define-lib
   ((comp-type library)
    (bin-type lib)
    (enable-formats byte native)))

(define-expand-rule ;depend-components
  (lambda (n v configs) (equal? n 'depend-components))
  (lambda (n v configs comps)
    (let ((additional-configs
            (map (lambda (d)
              (let* ((d (str d))
                     (comp-type `(get-var ,d "COMP_TYPE"))
                     (d-target  `(filename-join (get-var ,d "SRC_DIR") (get-var ,d "TARGET_NAME"))))
                `((merlin-include-dirs (e (if (string=? ,comp-type "library") (get-src-dirs ,d) '())))
                  (ocamlbuild-flags    (e (if (string=? ,comp-type "ppx") (format #f "-cflags -ppx,~a.byte" ,d-target) ""))))))
              v)))
      (fold (lambda (new-configs configs) (merge-configs configs new-configs)) configs additional-configs))))

(define-expand-rule ;use-bisect
  (lambda (n v configs) (and (equal? n 'enable-bisect) v))
  (lambda (n v configs comps)
    (let ((use-bisect `(get-var (get-current-component) "USE_BISECT")))
      (merge-configs configs
         `((ocaml-packages (e (if (not (string-empty? ,use-bisect)) 'bisect_ppx ""))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;  Functions for OCaml Makefile ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (get-src-dirs #:optional (c (get-current-component)))
  (find-subdirs (get-var c "SRC_DIR")))

(define* (get-test-dirs #:optional (c (get-current-component)))
  (find-subdirs (get-var c "TEST_DIR")))

(define* (get-options var #:optional (delim ",") (c (get-current-component)))
  (string-join (map str (get-var c var #t)) delim))

(define (get-module-names files-str)
  (let ((files (map basename (string-split files-str sp))))
    (for-each (lambda (f) (string-set! f 0 (char-upcase (string-ref f 0)))) files)
    files))

(define (generate-tags* c)
  (define target-bin    (filename-join (get-var c "SRC_DIR") (get-var c "TARGET_NAME")))
  (define test-bin      (filename-join (get-var c "TEST_DIR") (get-var c "TEST_NAME")))
  (define src-dirs      (get-src-dirs c))
  (define test-dirs     (get-test-dirs c))
  (define tags-adds     (get-var c "TAGS_ADDITIONALS" #t))
  (define wrap          (lambda (f o) (format #f "~a(~a)" f o)))
  (define syntax-pp     (string-join (map (lambda (x) (wrap "syntax"  x)) (get-var c "OCAML_SYNTAX_PP" #t)) ", "))
  (define packages      (string-join (map (lambda (x) (wrap "package" x)) (get-var c "OCAML_PACKAGES" #t)) ", "))
  (define test-packages (string-join (map (lambda (x) (wrap "package" x)) (get-var c "OCAML_TEST_PACKAGES" #t)) ", "))
  (define src-tags      (string-join (remove string-empty? (list syntax-pp packages)) ", "))
  (define test-tags     (string-join (remove string-empty? (list syntax-pp packages test-packages)) ", "))
  (with-output-to-string (lambda ()
    (format #t "# tags for ~a~%" c)
    (for-each (lambda (d) (format #t "<~a> : include~%" d)) (append src-dirs test-dirs))
    (if (not (string-empty? src-tags))
      (begin (for-each (lambda (d) (format #t "<~a/*.{ml,mli}> : ~a~%" d src-tags)) src-dirs)
             (format #t "<~a.{byte,native,cma,cmxa}> : ~a~%" target-bin src-tags)))
    (if (not (string-empty? test-tags))
      (begin (for-each (lambda (d) (format #t "<~a/*.{ml,mli}> : ~a~%" d test-tags)) test-dirs)
             (format #t "<~a.{byte,native,cma,cmxa}> : ~a~%" test-bin test-tags)))
    (for-each (lambda (l) (format #t "~a~%" l)) tags-adds))))

(define (generate-tags)
  (with-output-to-string (lambda ()
    (format #t "# DO NOT EDIT THIS FILE. It is generated every make process.~%")
    (for-each (lambda (s) (display s)) (map generate-tags* (get-components))))))

(define* (generate-merlin dest prj dirs packages exts #:optional (c (get-current-component)))
  (define proj-root     (get-rel-prj-root dest))
  (define build-dir     (get-var c "BUILD_DIR"))
  (with-output-to-string (lambda ()
    (format #t "PRJ ~a~%" prj)
    (for-each (lambda (d) (format #t "S ~a~%" (filename-join proj-root d))) dirs)
    (for-each (lambda (d) (format #t "B ~a~%" (filename-join proj-root build-dir d))) dirs)
    (for-each (lambda (p) (format #t "PKG ~a~%" p)) packages)
    (for-each (lambda (l) (format #t "EXT ~a~%" l)) exts))))

(define* (generate-src-merlin #:optional (c (get-current-component)))
  (generate-merlin (get-var c "SRC_DIR")
                   (get-var c "TARGET_NAME")
                   (append (get-src-dirs c) (get-var c "MERLIN_INCLUDE_DIRS" #t))
                   (get-var c "OCAML_PACKAGES" #t)
                   (get-var c "MERLIN_EXTS" #t)))

(define* (generate-test-merlin #:optional (c (get-current-component)))
  (generate-merlin (get-var c "TEST_DIR")
                   (get-var c "TEST_NAME")
                   (append (get-src-dirs c) (get-test-dirs c) (get-var c "MERLIN_INCLUDE_DIRS" #t))
                   (append (get-var c "OCAML_PACKAGES" #t) (get-var c "OCAML_TEST_PACKAGES" #t))
                   (get-var c "MERLIN_EXTS" #t)))

(define* (get-component-deps bin-fmt #:optional (c (get-current-component)))
  (let ((deps (get-var c "DEPEND_COMPONENTS" #t))
        (bin-type (if (string-empty? (get-var c "DEBUG")) "release" "debug")))
    (map (lambda (d)
      (let ((type (get-var d "COMP_TYPE")))
        (match type
          ("library" (format #f "~a-~a-~a" d bin-fmt bin-type))
          ("ppx"     (format #f "~a-byte-~a" d bin-type))
          ("program" (format #f "~a-~a-~a" d bin-fmt bin-type)))))
       deps)))

(define (meta-comp->pkg c) (str (get-var c "META_PACKAGE_NAME" #:default c)))

(define (meta-main-pkg)
  (let ((main-components (filter (lambda (c) (get-config c "META_MAIN_PACKAGE")) (get-components))))
    (if (null? main-components)
      (meta-comp->pkg DEFAULT_COMPONENT)
      (meta-comp->pkg (car main-components)))))

(define (meta-sub-pkgs)
  (remove (lambda (c) (equal? c (meta-main-pkg)))
          (map meta-comp->pkg (get-components))))

(define (meta-get-requires component)
  (let* ((ocaml-packages (map str (get-var component "OCAML_PACKAGES" #t)))
         (component-deps (map str (get-var component "DEPEND_COMPONENTS" #t)))
         (main-pkg       (str (meta-main-pkg)))
         (lib-comp-deps  (filter (lambda (c) (string=? "library" (str (get-var c "COMP_TYPE")))) component-deps))
         (lib-comp-deps  (delete-duplicates (map meta-comp->pkg lib-comp-deps)))
         (lib-comp-deps  (map (lambda (c) (if (string=? main-pkg c) main-pkg (format #f "~a.~a" main-pkg c))) lib-comp-deps)))
    (append ocaml-packages lib-comp-deps)))

(define (meta-generate)
  (define (gen-mata-var rule c)
    (cond
      ((equal? rule 'description)    (list "description" (get-var c "DESCRIPTION" #:default "no description")))
      ((equal? rule 'version)        (list "version" (get-var c "VERSION" #:default "0.1")))
      ((equal? rule 'ppx)            (list "ppx" (format #f "./~a.byte" (get-var c "TARGET_NAME"))))
      ((equal? rule 'requires)       (list "requires" (string-join (meta-get-requires c) " ")))
      ((equal? rule 'archive-byte)   (list "archive(byte)" (format #f "~a.cma" (get-var c "TARGET_NAME"))))
      ((equal? rule 'archive-native) (list "archive(native)" (format #f "~a.cmxa" (get-var c "TARGET_NAME"))))
      ((procedure? rule)             (rule c))
      (#t                            (gmk-eval "$$(error invalid meta generation rule)"))))
  (define (pkg-conf pkgs c rules)
    (let ((meta-pkg (meta-comp->pkg c)))
      (fold (lambda (rule pkgs)
        (let ((r (gen-mata-var rule c)))
          (alist-update meta-pkg (alist-append-value (car r) (cdr r) (alist-assoc-value meta-pkg pkgs)) pkgs)))
        pkgs rules)))
  (define (emit-meta-vars pkg-configs)
    (for-each (lambda (config)
      (format #t "~a = \"~a\"~%" (car config) (string-join (cdr config) "")))
      (reverse pkg-configs)))
  (let* ((pkg-configs (fold (lambda (c pkgs)
            (let ((rules (get-var c "META_RULES" #t))
                  (a-rules (get-var c "META_ADDITIONAL_RULES" #t))
                  (comp-type (str (get-var c "COMP_TYPE" #:default "program")))
                  (skip-meta? (not (string-empty? (get-var c "META_SKIP")))))
              (cond
                (skip-meta? pkgs)
                ((string=? comp-type "ppx")
                 (pkg-conf pkgs c (if (null? rules) (append '(version description ppx) a-rules) rules)))
                ((string=? comp-type "library")
                 (pkg-conf pkgs c (if (null? rules) (append '(version description requires archive-byte archive-native) a-rules) rules)))
                (#t
                 (pkg-conf pkgs c (if (null? rules) (append '(version description) a-rules) rules))))))
            '() (get-components))))
    (with-output-to-string (lambda ()
      (emit-meta-vars (alist-assoc-value (meta-main-pkg) pkg-configs))
      (newline)
      (for-each (lambda (sub-pkg)
        (let ((sub-pkg-configs (alist-assoc-value sub-pkg pkg-configs)))
          (if (not (null? sub-pkg-configs))
            (begin
              (format #t "package \"~a\" (~%" sub-pkg)
              (emit-meta-vars sub-pkg-configs)
              (format #t ")~%~%")))))
        (meta-sub-pkgs))))))


(define (build-sh-generate dest)
  (let ((op (open-file dest "a"))
        (c (get-current-component)))
    (let ((flags           (get-var c "_OCB_FLAGS"))
          (jsoo-flags      (get-var c "_JSOO_FLAGS"))
          (target-byte-out (get-var c "_TARGET_BYTE_OUTPUT"))
          (target-js       (get-var c "_TARGET_JS"))
          (target-byte     (get-var c "_TARGET_BYTE"))
          (target-native   (get-var c "_TARGET_NATIVE"))
          (enable-formats  (map str (get-var c "ENABLE_FORMATS" #t))))
      (if (member "byte"   enable-formats) (format op "ocamlbuild ~a ~a~%" flags target-byte))
      (if (member "native" enable-formats) (format op "ocamlbuild ~a ~a~%" flags target-native))
      (if (member "js"     enable-formats) (format op "js_of_ocaml ~a -o ~a ~a~%" jsoo-flags target-js target-byte-out)))
    (close-port op)))

#f
endef
#;scheme@@

##################################################
.DEFAULT_GOAL        = default
OCAMLBUILD           = ocamlbuild
TOUCH                = touch
MKDIR                = mkdir -p
RM                   = rm -f
RMDIR                = rm -fr
CP                   = cp
MAKE                 = make
_MAKE                = $(MAKE) C=$(C)
JS_OF_OCAML          = js_of_ocaml
JS                   = node
BISECT               = bisect-report
DEV_NULL             = /dev/null

############ default variables #############
MAIN_TARGETS         = main-byte-release
CHECK_TARGETS        = main-check-byte

SRC_DIR              = src
TEST_DIR             = test
BUILD_DIR            = _build

TARGET_NAME          = program
TEST_NAME            = test_all

JS_OF_OCAML_FLAGS       = --opt 3
JS_OF_OCAML_FLAGS_DEBUG = \
	--disable shortvar\
	--disable inline\
	--disable optcall\
	--enable pretty\
	--enable debuginfo\

TEST_JS_OF_OCAML_FLAGS  = \
	$(JS_OF_OCAML_FLAGS_DEBUG)\
	--disable genprim

COVERAGE_DIR         = $(BUILD_DIR)/coverage
BISECT_FILE          = $(COVERAGE_DIR)/bisect

######### include project local settings ##########
$(guile $(guile_lib))

_LOCAL_CONF = project.scm
$(guile (primitive-load "$(_LOCAL_CONF)"))

$(guile (check-config))

$(guile (define-makefile-vars (quote (MAIN_TARGETS CHECK_TARGETS BIN_TYPE TARGET_NAME TEST_NAME))))
$(guile (define-makefile-vars (quote (SRC_DIR TEST_DIR BUILD_DIR BISECT_FILE COVERAGE_DIR))))
$(guile (define-makefile-vars (quote (OCAMLBUILD_FLAGS TARGET_DEPENDS USE_BISECT TAGS_NO_REGENERATE))))
$(guile (define-makefile-vars (quote (JS_OF_OCAML_FLAGS JS_OF_OCAML_FLAGS_DEBUG JS_PRIMITIVES TEST_JS_PRIMITIVES TEST_JS_OF_OCAML_FLAGS))))

COMPONENTS           := $(guile (get-components))
###################################################
#NULL                 =
#SPACE                = $(null) #
#COMMMA               = ,

_TARGET               = $(SRC_DIR)/$(TARGET_NAME)
_TARGET_BYTE          = $(_TARGET).$(_BYTE_EXT)
_TARGET_NATIVE        = $(_TARGET).$(_NATIVE_EXT)
_TARGET_BYTE_OUTPUT   = $(BUILD_DIR)/$(_TARGET_BYTE)
_TARGET_NATIVE_OUTPUT = $(BUILD_DIR)/$(_TARGET_NATIVE)

_TEST_TARGET          = $(TEST_DIR)/$(TEST_NAME)
_TEST_BYTE            = $(_TEST_TARGET).$(_TEST_BYTE_EXT)
_TEST_NATIVE          = $(_TEST_TARGET).$(_TEST_NATIVE_EXT)
_TEST_BYTE_OUTPUT     = $(BUILD_DIR)/$(_TEST_BYTE)
_TEST_NATIVE_OUTPUT   = $(BUILD_DIR)/$(_TEST_NATIVE)

_TARGET_JS            = $(TARGET_NAME).js
_TARGET_JS_DEBUG      = $(TARGET_NAME)-debug.js
_TEST_JS              = $(TEST_NAME).js

_SRCS                 = $(guile (map (lambda (d) (find-sources d ".ml")) (get-src-dirs)))
_TEST_SRCS            = $(guile (map (lambda (d) (find-sources d ".ml")) (get-test-dirs)))

_TEST_BYTE_EXT        = byte
_TEST_NATIVE_EXT      = native

ifeq ($(BIN_TYPE),exe)
_BYTE_EXT             = byte
_NATIVE_EXT           = native
_TARGET_DEPS          = $(TARGET_DEPENDS) _tags
endif
ifeq ($(BIN_TYPE),lib)
_BYTE_EXT             = cma
_NATIVE_EXT           = cmxa
_TARGET_DEPS          = mllib $(TARGET_DEPENDS) _tags
_LIB_MODULES          = $(guile (get-module-names "$(patsubst %.ml,%,$(_SRCS))"))
endif

_OCB_FLAGS           := -use-ocamlfind -build-dir $(BUILD_DIR) $(OCAMLBUILD_FLAGS)
_OCB_FLAGS           += -Xs "$(guile (get-options "EXCLUDE_DIRS"))"
#_OCB_FLAGS           += -libs "$(guile (get-options "OCAML_INTERNAL_LIBS"))"

_JSOO_FLAGS           = $(JS_OF_OCAML_FLAGS) $(JS_PRIMITIVES)
_JSOO_FLAGS_DEBUG     = $(JS_OF_OCAML_FLAGS_DEBUG) $(JS_PRIMITIVES)
_JSOO_FLAGS_TEST      = $(TEST_JS_OF_OCAML_FLAGS) $(JS_PRIMITIVES) $(TEST_JS_PRIMITIVES)

ifdef DEBUG
_OCB_FLAGS += -tag debug
endif

_BYTE_COMP_DEPS       = $(guile (get-component-deps "byte"))
_NATIVE_COMP_DEPS     = $(guile (get-component-deps "native"))
_JS_COMP_DEPS         = $(guile (get-component-deps "js"))

###################################################
.PHONY: mllib clean _clean_in default check env _env-in
.PHONY: _byte-release _native-release _js-release
.PHONY: _byte-debug _native-debug _js-debug
.PHONY: _test-byte _test-native _test-js
.PHONY: _run-test-byte _run-test-native _run-test-js

default:$(MAIN_TARGETS)
check:$(CHECK_TARGETS)

define define_pseudo_goals
(for-each (lambda (c) (gmk-eval (format #f "
~a-byte-release:
	@$(MAKE) C=\"~:*~a\" _byte-release
~:*~a-native-release:
	@$(MAKE) C=\"~:*~a\" _native-release
~:*~a-js-release:
	@$(MAKE) C=\"~:*~a\" _js-release
~:*~a-byte-debug:
	@$(MAKE) C=\"~:*~a\" _byte-debug
~:*~a-native-debug:
	@$(MAKE) C=\"~:*~a\" _native-debug
~:*~a-js-debug:
	@$(MAKE) C=\"~:*~a\" _js-debug
~:*~a-check-byte:
	@$(MAKE) C=\"~:*~a\" _run-test-byte DEBUG=true
~:*~a-check-native:
	@$(MAKE) C=\"~:*~a\" _run-test-native DEBUG=true
~:*~a-check-js:
	@$(MAKE) C=\"~:*~a\" _run-test-js DEBUG=true
"
  c))) (get-components))
#f
endef
$(guile $(define_pseudo_goals))

_byte-release:$(_TARGET_BYTE_OUTPUT)
_native-release:$(_TARGET_NATIVE_OUTPUT)
_js-release:$(_TARGET_JS)
_byte-debug:
	$(_MAKE) $(_TARGET_BYTE_OUTPUT) DEBUG=true

_native-debug:
	$(_MAKE) $(_TARGET_NATIVE_OUTPUT) DEBUG=true

_js-debug:
	$(_MAKE) $(_TARGET_JS_DEBUG) DEBUG=true

_run-test-byte:$(_TEST_BYTE_OUTPUT)
ifdef USE_BISECT
	$(RMDIR) $(COVERAGE_DIR)
	$(MKDIR) $(COVERAGE_DIR)
	BISECT_FILE=$(BISECT_FILE) ./$(_TEST_BYTE_OUTPUT)
	cd $(BUILD_DIR); $(BISECT) -html ../$(COVERAGE_DIR) ../$(BISECT_FILE)*.out
else
	./$(_TEST_BYTE_OUTPUT)
endif

_run-test-native:$(_TEST_NATIVE_OUTPUT)
ifdef USE_BISECT
	$(RMDIR) $(COVERAGE_DIR)
	$(MKDIR) $(COVERAGE_DIR)
	BISECT_FILE=$(BISECT_FILE) ./$(_TEST_NATIVE_OUTPUT)
	cd $(BUILD_DIR); $(BISECT) -html ../$(COVERAGE_DIR) ../$(BISECT_FILE)*.out
else
	./$(_TEST_NATIVE_OUTPUT)
endif
_run-test-js:$(_TEST_JS)
ifdef USE_BISECT
	$(RMDIR) $(COVERAGE_DIR)
	$(MKDIR) $(COVERAGE_DIR)
	BISECT_FILE=$(BISECT_FILE) $(JS) $(_TEST_JS)
	cd $(BUILD_DIR); $(BISECT) -html ../$(COVERAGE_DIR) ../$(BISECT_FILE)*.out
else
	$(JS) $(_TEST_JS)
endif

$(_TARGET_BYTE_OUTPUT):$(_SRCS) $(_TARGET_DEPS) $(_BYTE_COMP_DEPS)
	$(OCAMLBUILD) $(_OCB_FLAGS) $(_TARGET_BYTE)

$(_TARGET_NATIVE_OUTPUT):$(_SRCS) $(_TARGET_DEPS) $(_NATIVE_COMP_DEPS)
	$(OCAMLBUILD) $(_OCB_FLAGS) $(_TARGET_NATIVE)

$(_TARGET_JS):$(_TARGET_BYTE_OUTPUT) $(JS_PRIMITIVES) $(_JS_COMP_DEPS)
	$(JS_OF_OCAML) $(_JSOO_FLAGS) -o $@ $(_TARGET_BYTE_OUTPUT)

$(_TARGET_JS_DEBUG):$(_TARGET_BYTE_OUTPUT) $(JS_PRIMITIVES)
	$(JS_OF_OCAML) $(_JSOO_FLAGS_DEBUG) -o $@ $(_TARGET_BYTE_OUTPUT)

$(_TEST_BYTE_OUTPUT):$(_SRCS) $(_TEST_SRCS) _tags $(_BYTE_COMP_DEPS)
	$(OCAMLBUILD) $(_OCB_FLAGS) $(_TEST_BYTE)

$(_TEST_NATIVE_OUTPUT):$(_SRCS) $(_TEST_SRCS) _tags $(_NATIVE_COMP_DEPS)
	$(OCAMLBUILD) $(_OCB_FLAGS) $(_TEST_NATIVE)

$(_TEST_JS):$(_TEST_BYTE_OUTPUT) $(JS_PRIMITIVES) $(TEST_JS_PRIMITIVES) $(_JS_COMP_DEPS)
	$(JS_OF_OCAML) $(_JSOO_FLAGS_TEST) -o $@ $(_TEST_BYTE_OUTPUT)

mllib:
	$(file > $(_TARGET).mllib,# do not edit this file! it is generated every make process)
	$(foreach m,$(_LIB_MODULES),$(file >> $(_TARGET).mllib, $m))

ifndef TAGS_NO_REGENERATE # regenerate _tags if TAGS_NO_REGENERATE is not defined.
.PHONY: _tags
endif
_tags:$(_LOCAL_CONF)
	$(guile (with-output-to-file "_tags" (lambda () (display (generate-tags)))))

merlin:$(SRC_DIR)/.merlin $(TEST_DIR)/.merlin

$(SRC_DIR)/.merlin:$(_LOCAL_CONF)
	$(guile (with-output-to-file "$@" (lambda () (display (generate-src-merlin)))))

$(TEST_DIR)/.merlin:$(_LOCAL_CONF)
	$(guile (with-output-to-file "$@" (lambda () (display (generate-test-merlin)))))

env:
	@echo $(foreach comp,$(COMPONENTS),$(shell $(MAKE) C=$(comp) _env-in)) > /dev/null

_env-in:
	$(RM) $(SRC_DIR)/.merlin
	$(RM) $(TEST_DIR)/.merlin
	$(_MAKE) _tags
	$(_MAKE) merlin

clean::_clean_in

_clean_in:
	$(OCAMLBUILD) -clean
	$(RM) $(_TARGET_JS)
	$(RM) $(_TARGET_JS_DEBUG)
	$(RM) $(_TEST_JS)
	$(RM) $(CLEAN_ADDITIONAL)
	$(RMDIR) $(COVERAGE_DIR)

META:$(_LOCAL_CONF)
	$(guile (with-output-to-file "META" (lambda () (display (meta-generate)))))

build.sh:$(_LOCAL_CONF)
	$(file > build.sh, #!/bin/sh)
	@echo $(foreach c,$(COMPONENTS),$(shell $(MAKE) C=$(c) _build.sh))

_build.sh:
	$(guile (build-sh-generate "build.sh"))

dist:
	$(MAKE) check
	$(MAKE) env
	$(MAKE) META
	$(MAKE) build.sh
	$(MAKE) clean

# debugging of makefile
debug-makefile:
	$(guile (display (meta-generate)))
	$(guile (newline))
	$(guile (display (generate-tags)))
	$(guile (newline))
	$(guile (display (generate-src-merlin)))
	$(guile (newline))
	$(guile (display (generate-test-merlin)))
	$(guile (newline))
	$(guile (display (build-sh-generate)))

show-config:
	$(guile (display (get-components)))
	$(guile (pretty-print COMPONENTS))
