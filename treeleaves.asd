(require "asdf")
(asdf:defsystem "treeleaves"
  :version "0.3.1"
  :author "Joseph Diza <josephm.diza@gmail.com>"
  :license "AGPLv3"
  :depends-on (:cl-utilities :cl-ppcre
               :unix-opts :uiop
               :sxql :mito
               :iterate :str
               :log4cl)
  :components ((asdf:module "src"
                :components
                ((:file "format")
                 (:file "models")
                 (:file "cli")
                 (:file "treeleaves")
                 )))
  :description "Directory tag generator for files"
  :build-operation "program-op" ;; leave as is
  :build-pathname "bin/treeleaves"
  :entry-point "treeleaves:main"
  :in-order-to ((asdf:test-op (asdf:test-op "treeleaves/tests"))))

(asdf:defsystem "treeleaves/tests"
  :author "Joseph Diza <josephm.diza@gmail.com>"
  :license "AGPLv3"
  :depends-on (:treeleaves
               :fiveam)
  :components ((asdf:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for treeleaves"
  :perform (asdf:test-op (op c) (symbol-call :fiveam :run! (find-symbol* "treeleaves" "treeleaves/tests/main"))))
