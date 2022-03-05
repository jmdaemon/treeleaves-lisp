(defpackage treeleaves.format
  (:use :cl)
  (:documentation "Generate directory based file tags")
  (:export :split-dir
           :make-tag
           :print-tags
           :format-tags
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
