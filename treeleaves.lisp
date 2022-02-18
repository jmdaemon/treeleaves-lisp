#!/usr/bin/sbcl --script

(load "/usr/lib/quicklisp/setup.lisp")

; Imports
(require "uiop")
(require "cl-utilities")

; The directory to the pdf files
(defvar dir "~/Documents")

; Expands the directory path, and collects all the pdf files
(defparameter pdf-dir (concatenate 'string (uiop:native-namestring dir) "/**/*.pdf"))
(defparameter pdf-files (directory pdf-dir))

;Splits a directory file path on '/' characters
(defun split-dir (*dir*)
    (cl-utilities:split-sequence #\/ *dir*))

; Outputs the filepaths to the directories separated by whitespace
(loop for filepath in pdf-files
      do
      (defparameter split-filepath (split-dir (uiop:native-namestring filepath)))
      (defparameter tags (subseq split-filepath 4))
      (format t "~{~a~^ ~}~%" tags))
