(defsystem "treeleaves"
  :version "0.1.0"
  :author "Joseph Diza <josephm.diza@gmail.com>"
  :license "AGPLv3"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "treeleaves"))))
  :description ""
  :in-order-to ((test-op (test-op "treeleaves/tests"))))

(defsystem "treeleaves/tests"
  :author "Joseph Diza <josephm.diza@gmail.com>"
  :license "AGPLv3"
  :depends-on ("treeleaves"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for treeleaves"
  :perform (test-op (op c) (symbol-call :rove :run c)))
