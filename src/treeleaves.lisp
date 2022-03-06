(defpackage :treeleaves
  (:use :cl)
  (:documentation "Generate directory based file tags")
  (:import-from #:treeleaves.models
                #:connect
                #:ensure-tables
                #:write-to-db
                #:find-doc
                #:query
                #:document
                #:add-to-db)
  (:import-from #:treeleaves.format
                #:split-dir
                #:make-tag
                #:print-tags
                #:format-tags
                #:fmt)
  (:export :main))
(in-package :treeleaves)

(require "uiop") 
(require "unix-opts") 

; TODO:
; - Add table cli argument
;   - Replace hardcoded 'document calls with table arg
; - Add subcommand feature to find dead filepaths and remove them from the database (reindex)
; - Add option to only generate and print file tags, file path for a given file

(defun main ()
  "Main application entry point"

; Define Args
(opts:define-opts
    (:name :help
           :description "Show this help message"
           :short #\h
           :long "help")
    (:name :fp
           :description "System directory to generate tags for"
           :short #\d
           :long "directory")
    (:name :o
           :description "System file path for the generated database"
           :short #\o
           :long "output")
    (:name :p
           :description "Globbing pattern used to search for files in the directory"
           :short #\p
           :long "pattern" )
    (:name :q
           :description "Query the database for files"
           :short #\q
           :long "query"))

(defun show-usage ()
  (progn
    (opts:describe
      :prefix "Generate directory based file tags"
      :args "[keywords]")
    (opts:exit))) 

; Parse CLI Opts
(multiple-value-bind (options free-args)
  (opts:get-opts (uiop:command-line-arguments))
  (if (getf options :help)
      (show-usage))
  (if (getf options :fp)
      (defparameter dir (format t "~a~&" free-args))
      (defparameter dir "~/Documents"))
  (if (getf options :o)
      (defparameter db (format t "~a~&" free-args))
      (defparameter db "documents.sqlite"))
  (if (getf options :p)
      (defparameter pat (format t "~a~&" free-args))
      (defparameter pat "/**/*.pdf"))
  (if (getf options :q)
      (query db (list 'document) free-args))
  )

; Set defaults
(if (eql dir "~/Documents")     (format t "Using default directory: ~a~%" dir))
(if (eql db "documents.sqlite") (format t "Using default database name: ~a~%" db))
(if (eql pat "/**/*.pdf")       (format t "Globbing pattern: ~a~%" pat))

; Expands the directory path, and collects all the files
(defparameter file-dir (concatenate 'string (uiop:native-namestring dir) pat))
(defparameter files (directory file-dir))

; Connect and write to database
(connect db)
(ensure-tables (list 'document))
(add-to-db 'document files))
