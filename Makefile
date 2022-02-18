LISP ?= sbcl
QL ?= /usr/lib/quicklisp/setup.lisp

#build:
	#sbcl --load treeleaves.asd \
			 #--eval '(ql:quickload :treeleaves)' \
         #--eval "(sb-ext:save-lisp-and-die #p\"treeleaves\" :toplevel #'treeleaves:treeleaves :executable t)"

#build:
	#sbcl --load treeleaves.asd \
			 #--eval "(load $(QL)" \
			 #--eval '(ql:quickload :treeleaves)' \
			 #--eval "(sb-ext:save-lisp-and-die #p\"treeleaves\" :toplevel #'treeleaves:main :executable t)"

#build:
	#$(LISP) --load treeleaves.asd \
		#--eval '(asdf:load-system :treeleaves)' \
		#--eval '(ql:quickload :treeleaves)' \
		#--eval '(asdf:make :treeleaves)' \
		#--eval '(quit)'
#build:
	#$(LISP) --load treeleaves.asd \
		#--eval '(ql:quickload :treeleaves)' \
		#--eval '(ql:quickload "asdf")' \
		#--eval '(asdf:load-system :treeleaves)' \
		#--eval '(asdf:make :treeleaves)' \
		#--eval '(quit)'
		#--eval '(ql:quickload :treeleaves)' \
		--eval '(asdf:make :treeleaves)' \
		--eval '(quit)'

#build:
	#sbcl --load treeleaves.asd \
			 #--eval '(ql:quickload :treeleaves)' \
			 #--eval "(sb-ext:save-lisp-and-die #p\"bin\treeleaves\" :toplevel #'treeleaves:main :executable t)"

# --eval '(ql:quickload "asdf")' \

build:
	$(LISP) --load treeleaves.asd \
		--eval '(ql:quickload :treeleaves)' \
		--eval '(asdf:make :treeleaves)' \
		--eval '(quit)'
