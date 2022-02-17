#!/usr/bin/sbcl --script

(require "uiop")
(defvar dir "~/Documents")

;(defparameter *dirs* nil "All recursive directories.")

;(uiop:collect-sub*directories dir
    ;(constantly t)
    ;(constantly t)
    ;(lambda (it) (push it *dirs*)))
;(print *dirs*)

; Expands the directory path, and collects all the pdf files
(defparameter pdf_files (concatenate 'string (uiop:native-namestring dir) "/**/*.pdf"))
(directory pdf_files)
