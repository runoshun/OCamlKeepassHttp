############## main configurations ###############
TARGET_NAME  := index
TEST_NAME    := test_all

MAIN_TARGET  = release-js
CHECK_TARGET = $(CHECK_JS_TARGET)

# source and test-directories
SRC_DIR      = src-ocaml
TEST_DIR     = test-ocaml

SRC_DIRS=\
	$(SRC_DIR) \
	$(SRC_DIR)/backend \
	$(SRC_DIR)/backend/js \

TEST_DIRS=\
	$(TEST_DIR) \
	$(TEST_DIR)/js

OCAML_PACKAGES=\
	js_of_ocaml \
	yojson \
	base64 \
	ppx_deriving.std

OCAML_TEST_PACKAGES=\
	oUnit

# additional contents _tags and .merlin
TAGS_ADDITIONAL=\
<$(SRC_DIR)/$(TARGET_NAME).ml> : package(js_of_ocaml.syntax),syntax(camlp4o)\n\
<$(SRC_DIR)/backend/js/*.{ml,mli}>   : package(js_of_ocaml.syntax),syntax(camlp4o)\n\
<$(TEST_DIR)/js/*.{ml,mli}>   : package(js_of_ocaml.syntax),syntax(camlp4o)\n\

MERLIN_ADDITIONAL=\
EXT js\n\

OCAMLBUILD_FLAGS     += -use-ocamlfind -X semantic -X node_modules

JS_OF_OCAML_FLAGS       += --opt 3
JS_OF_OCAML_FLAGS_DEBUG += --disable shortvar --pretty --disable inline --disable optcall --enable debuginfo
TEST_JS_OF_OCAML_FLAGS  += $(JS_OF_OCAML_FLAGS_DEBUG) --disable genprim

JS_PRIM_FILES            =
JS_TEST_PRIM_FILES       = $(TEST_DIR)/prim_unix.js

##################################################

BUILD_DIR            := _build
SRCS                 := $(shell find $(SRC_DIR))
TEST_SRCS            := $(shell find $(TEST_DIR))

TARGET               := $(SRC_DIR)/$(TARGET_NAME)
TARGET_BYTE          := $(TARGET).byte
TARGET_NATIVE        := $(TARGET).native
TARGET_BYTE_OUTPUT   := $(BUILD_DIR)/$(TARGET_BYTE)
TARGET_NATIVE_OUTPUT := $(BUILD_DIR)/$(TARGET_NATIVE)

TEST_TARGET          := $(TEST_DIR)/$(TEST_NAME)
TEST_BYTE            := $(TEST_TARGET).byte
TEST_NATIVE          := $(TEST_TARGET).native
TEST_BYTE_OUTPUT     := $(BUILD_DIR)/$(TEST_BYTE)
TEST_NATIVE_OUTPUT   := $(BUILD_DIR)/$(TEST_NATIVE)

CHECK_BYTE_TARGET    := .check-byte
CHECK_NATIVE_TARGET  := .check-native
CHECK_JS_TARGET      := .check-js

OCAMLBUILD           := ocamlbuild
TOUCH                := touch
MKDIR                := mkdir -p
RM                   := rm -f
MAKE                 := make
OCAML                := ocaml

NULL                 :=
SPACE                := $(null) #
COMMMA               := ,

TAGS_OCAML_PACKAGES      := $(subst $(SPACE),$(COMMMA),$(strip $(foreach pack,$(OCAML_PACKAGES),package($(pack)))))
TAGS_OCAML_TEST_PACKAGES := $(subst $(SPACE),$(COMMMA),$(strip $(foreach pack,$(OCAML_TEST_PACKAGES),package($(pack)))))

CHECK_LOG=/tmp/$(TARGET_NAME)-check.log
TEST_SETUP_SCRIPT=test-data/test-setup.ml

# vars for js_of_ocaml
TARGET_JS       := $(TARGET_NAME).js
TARGET_JS_DEBUG := $(TARGET_NAME)-debug.js
TEST_JS         := $(TEST_NAME).js

JS_OF_OCAML := js_of_ocaml
NODE        := node

###################################################

.PHONY: run-byte run-native
.PHONY: env check-cont rebuild recheck clean

all:$(MAIN_TARGET)

init:_tags .merlin $(TEST_DIRS) $(SRC_DIRS) $(TARGET).ml $(TEST_TARGET).ml

check:$(CHECK_TARGET)

release-byte:$(TARGET_BYTE_OUTPUT)

release-native:$(TARGET_NATIVE_OUTPUT)

test-byte:$(TEST_BYTE_OUTPUT)

test-native:$(TEST_NATIVE_OUTPUT)

debug-byte:
	$(MAKE) $(TARGET_BYTE_OUTPUT) OCAMLBUILD_FLAGS="-tag debug $(OCAMLBUILD_FLAGS)"

debug-native:$(TARGET_NATIVE_OUTPUT)
	$(MAKE) $(TARGET_NATIVE_OUTPUT) OCAMLBUILD_FLAGS="-tag debug $(OCAMLBUILD_FLAGS)"

run-byte: $(TARGET_BYTE_OUTPUT)
	./$(TARGET_BYTE_OUTPUT) $(RUN_PARAMS)

run-native: $(TARGET_NATIVE_OUTPUT)
	./$(TARGET_NATIVE_OUTPUT) $(RUN_PARAMS)

$(CHECK_BYTE_TARGET): $(TEST_BYTE_OUTPUT)
	./$(TEST_BYTE_OUTPUT)
	$(TOUCH) $@

$(CHECK_NATIVE_TARGET): $(TEST_NATIVE_OUTPUT)
	./$(TEST_NATIVE_OUTPUT)
	$(TOUCH) $@

check-cont:
	@while true; do \
		$(MAKE) $(CHECK_TARGET) 2>/dev/null > $(CHECK_LOG); \
		grep -e "\(Failure:\|Error:\)" -B1 -A3 $(CHECK_LOG); \
		grep -e "Ran" -A1 $(CHECK_LOG); \
		grep -e "Compilation unsuccessful" -B5 $(CHECK_LOG); \
		rm -f $(CHECK_LOG); \
		sleep 3; \
	done

rebuild:
	$(MAKE) clean
	$(MAKE) env
	$(MAKE) all

recheck:
	$(RM) $(CHECK_TARGET)
	$(MAKE) $(CHECK_TARGET)

env:
	$(RM) _tags
	$(RM) .merlin
	$(MAKE) _tags
	$(MAKE) .merlin

clean:
	$(OCAMLBUILD) -clean
	$(RM) $(TARGET_JS)
	$(RM) $(TARGET_JS_DEBUG)
	$(RM) $(TEST_JS)
	$(RM) $(CHECK_LOG)
	$(RM) $(CHECK_NATIVE_TARGET)
	$(RM) $(CHECK_BYTE_TARGET)
	$(RM) $(CHECK_JS_TARGET)

# file specific rules

$(SRC_DIRS) $(TEST_DIRS):
	$(MKDIR) $@

$(TARGET).ml $(TEST_TARGET).ml:
	$(TOUCH) $@

$(TARGET_BYTE_OUTPUT):$(SRCS)
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $(TARGET_BYTE)

$(TARGET_NATIVE_OUTPUT):$(SRCS)
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $(TARGET_NATIVE)

$(TEST_BYTE_OUTPUT):$(SRCS) $(TEST_SRCS) _tags
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) -tag debug $(TEST_BYTE)

$(TEST_NATIVE_OUTPUT):$(SRCS) $(TEST_SRCS) _tags
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $(TEST_NATIVE)

_tags:
	@echo -e "generate _tags ..."
	@for dir in $(SRC_DIRS); do \
		echo -e "<$$dir> : include" >> _tags; \
		echo -e "<$${dir}/*.{ml,mli}> : $(TAGS_OCAML_PACKAGES)" >> _tags; \
	done
	@for dir in $(TEST_DIRS); do \
		echo -e "<$$dir> : include" >> _tags; \
		echo -e "<$${dir}/*.{ml,mli}> : $(TAGS_OCAML_PACKAGES)" >> _tags; \
		echo -e "<$${dir}/*.{ml,mli}> : $(TAGS_OCAML_TEST_PACKAGES)" >> _tags; \
	done
	@echo -e "<$(TARGET).{byte,native}> : $(TAGS_OCAML_PACKAGES)" >> _tags
	@echo -e "<$(TEST_TARGET).{byte,native}> : $(TAGS_OCAML_PACKAGES)" >> _tags
	@echo -e "<$(TEST_TARGET).{byte,native}> : $(TAGS_OCAML_TEST_PACKAGES)" >> _tags
	@echo -e "$(TAGS_ADDITIONAL)" >> _tags

.merlin:
	@echo -e "generate .merlin ..."
	@echo -e "PRJ $(TARGET_NAME)" >> .merlin
	@for dir in $(SRC_DIRS) $(TEST_DIRS); do \
		echo -e "S $$dir" >> .merlin; \
	done
	@for dir in $(SRC_DIRS) $(TEST_DIRS); do \
		echo -e "B $(BUILD_DIR)/$$dir" >> .merlin; \
	done
	@for pack in $(OCAML_PACKAGES) $(OCAML_TEST_PACKAGES); do \
		echo -e "PKG $${pack}" >> .merlin; \
	done
	@echo -e "$(MERLIN_ADDITIONAL)" >> .merlin


# rules for js_of_ocaml
release-js:$(TARGET_JS)

debug-js:$(TARGET_JS_DEBUG)

test-js:$(TEST_JS)

run-js:$(TARGET_JS)
	$(NODE) $(TARGET_JS) $(RUN_PARAMS)

$(CHECK_JS_TARGET):$(TEST_JS)
	$(OCAML) $(TEST_SETUP_SCRIPT)
	$(NODE) $(TEST_JS)
	$(TOUCH) $@

$(TARGET_JS):$(TARGET_BYTE_OUTPUT)
	$(JS_OF_OCAML) $(JS_OF_OCAML_FLAGS) -o $@ $(JS_PRIM_FILES) $(TARGET_BYTE_OUTPUT)

$(TARGET_JS_DEBUG):$(TARGET_BYTE_OUTPUT)
	$(JS_OF_OCAML) $(JS_OF_OCAML_FLAGS_DEBUG) -o $@ $(JS_PRIM_FILES) $(TARGET_BYTE_OUTPUT)

$(TEST_JS):$(TEST_BYTE_OUTPUT)
	$(JS_OF_OCAML) $(TEST_JS_OF_OCAML_FLAGS) -o $@ $(JS_PRIM_FILES) $(JS_TEST_PRIM_FILES) $(TEST_BYTE_OUTPUT)

