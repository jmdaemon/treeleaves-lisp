LISP ?= sbcl
EXE = treeleaves
QL = /usr/lib/quicklisp/setup.lisp

BIN_PREFIX = bin
BUILD_EXEC = $(BIN_PREFIX)/$(EXE)

# Set install path
ifeq ($(PREFIX),)
    PREFIX := /usr/local
endif

# Rules
build:
	$(LISP) --load $(QL) \
		--eval "(load \"$(EXE).asd\")" \
		--eval "(ql:quickload :$(EXE))" \
		--eval "(asdf:make :$(EXE))" \
		--eval "(asdf:test-system :$(EXE))" \
		--eval "(quit)"

install: build $(BUILD_EXEC)
	install $(BUILD_EXEC) $(DESTDIR)$(PREFIX)/bin/$(EXE)
