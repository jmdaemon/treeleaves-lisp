#!/usr/bin/sbcl --script

; Imports
(require "uiop")
(require "cl-utilities")

; The directory to the pdf files
(defvar dir "~/Documents")

; Expands the directory path, and collects all the pdf files
(defparameter pdf_dir (concatenate 'string (uiop:native-namestring dir) "/**/*.pdf"))
(defparameter pdf_files (directory pdf_dir))

; Split on '/' characters
(cl-utilities:split-sequence #\/ dir)
