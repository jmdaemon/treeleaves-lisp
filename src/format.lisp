(defpackage treeleaves.format
  (:use :cl)
  (:documentation "Generate directory based file tags")
  (:export :split-dir
           :make-tag
           :print-tags
           :format-tags
           :find-tables
           :format-args
           :parse-database-args
           :fmt
           ))
(in-package :treeleaves.format)

(require "uiop") 
(require "str")
(require "cl-utilities")
(require "cl-ppcre")

; Functions
(defun split-dir (dir)
    "Splits a directory file path on '/' characters"
    (cl-utilities:split-sequence #\/ dir))

(defun make-tag (filepath)
  "Returns a list of directory names

   Note that this function is hardcoded to ignore the first four directory
   names in the list
   "
  (defparameter split-filepath (split-dir (uiop:native-namestring filepath)))
  (subseq split-filepath 4))

(defun print-tags (tags) 
  "Pretty prints a list of tags"
  (format t "~{~a~^ ~}~%" tags))

(defun format-tags (tags) 
  "Formats a list of tags to a string"
  (format NIL "~{~a~^ ~}~%" tags))

(defun fmt (text)
  "Format some text into a string"
  (format nil "~A" text)) 

(defun format-args (argv) 
  "Parses a list of arguments into a string"
  (format NIL "~{~a~^ ~}" argv))

(defun find-tables (str)
  "Finds and returns all tables found in str"
  (ppcre:register-groups-bind (matches)
                              ;("([\\w ]*)" str :sharedp t)
                              ;("(-t) ([\\w ]*)" str :sharedp t)
                              ;("(t [\\w ]*)" str :sharedp t)
                              ("-t ([\\w ]*)" str :sharedp t)
                              (remove nil matches)))

;(find-tables "document -f ./documents.sqlite -q :tags Books %") 
;(find-tables "-t document -f ./documents.sqlite -q :tags Books %") 
;(find-tables "./bin/treeleaves -t document -f ./documents.sqlite -q \":tags\" \"Books\" %") 

(defun parse-tables (args)
  "Returns a list of the database tables found in free-args"

  ; Make sure free-args is converted to a string
  (defparameter tables-string (str:trim-right (find-tables args)))
  (log:info "Result from find-tables: " tables-string)

  (defparameter tables-stringlist (cl-utilities:split-sequence #\Space tables-string))
  (log:info "Result from split: " tables-stringlist)

  (defparameter tables (list (find-class 'document)))
  (log:info "Result from tables: " tables)
  (remove nil tables))

; Test 
;(defparameter db "documents.sqlite")
;(defparameter tbls (parse-tables "-t document books"))
;tbls
;(initdb db tbls)
;(querydb tbls ":tags" "Books %")
;(initdb "documents.sqlite" (list 'document))
;;(querydb (list 'document) ":tags" "Books %")

(defun parse-search-args (str)
  "Parses a string of command line arguments into
   a keyword and a search term"
    (ppcre:register-groups-bind (*keyword* *search-term*)
                                ("(:[\\w]*) ([\\w %]*)" str :sharedp t)
                                (list *keyword* *search-term*))) 
; Test
;(parse-search-term ":tags Books %")
;(parse-search-term "document -f ./documents.sqlite -qa :tags Books %") 

(defun parse-database-args (str)
  "Parses a string of command line arguments into
   a keyword and a search term"
    (ppcre:register-groups-bind (matches)
                                ;("-f ([\\w\./]*[sqlite])" str :sharedp t)
                                ("-f ([\\~\\w\\./]*[sqlite])" str :sharedp t)
                                (remove nil matches))) 
; Test
;(setq cwd (parse-database-args "document -f ./documents.sqlite -qa :tags Books %"))
;(setq docs-dir (parse-database-args "document -f ~/Documents/documents.sqlite -qa :tags Books %"))
;(setq dir-namestring (uiop:native-namestring cwd))
;(uiop:native-namestring dir-namestring)
