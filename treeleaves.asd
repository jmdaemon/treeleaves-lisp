(require "asdf")
(asdf:defsystem "treeleaves"
  :version "0.2.0"
  :author "Joseph Diza <josephm.diza@gmail.com>"
  :license "AGPLv3"
  :depends-on (:uiop :cl-utilities :mito :unix-opts :iterate)
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
  :depends-on ("treeleaves"
               "rove")
  :components ((asdf:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for treeleaves"
  :perform (asdf:test-op (op c) (symbol-call :rove :run c)))
