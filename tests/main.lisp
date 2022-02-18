(defpackage treeleaves/tests/main
  (:use :cl
        :treeleaves
        :rove))
(in-package :treeleaves/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :treeleaves)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
