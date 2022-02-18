#!/usr/bin/sbcl --script

; Imports
(require "uiop")
(require "cl-utilities")

; The directory to the pdf files
(defvar dir "~/Documents")

; Expands the directory path, and collects all the pdf files
(defparameter pdf-dir (concatenate 'string (uiop:native-namestring dir) "/**/*.pdf"))
(defparameter pdf-files (directory pdf-dir))

(defun split-dir (dirname)
  "Splits a directory file path on '/' characters"
    (cl-utilities:split-sequence #\/ dirname))

(loop for filepath in pdf-files
  do (print (split-dir (uiop:native-namestring filepath))))
