(defpackage treeleaves.format
  (:use :cl)
  (:documentation "Generate directory based file tags")
  (:export :split-dir
           :make-tag
           :print-tags
           :format-tags
           :format-args
           :fmt
           ))
(in-package :treeleaves.format)

(require "uiop") 
(require "cl-utilities")

; Functions
; Splits a directory file path on '/' characters
(defun split-dir (*dir*)
    (cl-utilities:split-sequence #\/ *dir*))

(defun make-tag (filepath)
  (defparameter split-filepath (split-dir (uiop:native-namestring filepath)))
  (subseq split-filepath 4))

(defun print-tags (tags) 
  (format t "~{~a~^ ~}~%" tags))

(defun format-tags (tags) 
  (format NIL "~{~a~^ ~}~%" tags))

(defun fmt (text)
  (format nil "~A" text)) 

(defun format-args (argv) 
  "Parses a list of arguments into a string"
  (format NIL "~{~a~^ ~}" argv))
