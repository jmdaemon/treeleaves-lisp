#!/usr/bin/sbcl --script

; Imports
(require "uiop")
(require "cl-utilities")

; The directory to the pdf files
(defvar dir "~/Documents")

; Expands the directory path, and collects all the pdf files
(defparameter pdf-dir (concatenate 'string (uiop:native-namestring dir) "/**/*.pdf"))
(defparameter pdf-files (directory pdf-dir))

; Split on '/' characters
(cl-utilities:split-sequence #\/ dir)
