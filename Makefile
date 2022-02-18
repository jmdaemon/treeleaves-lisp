LISP ?= sbcl

build:
	$(LISP) --load treeleaves.asd \
		--eval '(ql:quickload :treeleaves)' \
		--eval '(asdf:make :treeleaves)' \
		--eval '(quit)'
