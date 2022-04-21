(defpackage treeleaves/tests/main
  (:use :cl
        :treeleaves
        :fiveam))
(in-package :treeleaves/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :treeleaves)' in your Lisp.

;(deftest test-target-1
  ;(testing "should (= 1 1) to be true"
    ;(ok (= 1 1))))

; Tests
(require "fiveam")
(require "treeleaves")
;(use-package :fiveam)

; Treeleaves Suite
(def-suite treeleaves
  :description "Main Treeleaves Tests Suite")

; Treeleaves Models Suite
(def-suite treeleaves.models
  :description "Treeleaves.Models Tests Suite"
  :in treeleaves)

(in-suite treeleaves.models)

; Index into the database table types
(test database-table-types-index-with-document
  (let ((result (gethash "document" *database-table-types*)))
    ;; Tip: put the expected value as the first argument of = or equal, string= etc.
    ;; FiveAM generates a more readable report following this convention.
    (is (equal 'document result) "*database-table-types* contains document class"))
  )

; Run test suite manually
;(run! 'treeleaves)

; treeleaves.model
; find-docs
;(defparameter db "documents.sqlite")
;(defparameter tables (list 'document))
;(connect db)
;(ensure-tables tables)
;(find-docs :query "Books")

; query-db
; query-db-all
