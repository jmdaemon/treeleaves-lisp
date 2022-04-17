(defpackage :treeleaves
  (:use :cl :treeleaves.models)
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
                #:format-args
                #:fmt)
  (:import-from #:treeleaves.cli
                #:build-cli
                #:show-usage
                #:show-verbose
                #:parse-opts)
  (:export :main))
(in-package :treeleaves)

(require "uiop") 
(require "unix-opts") 

; TODO:
; Pruning:
; - Find dead filepaths and remove them from the database (reindex)
; Indexing
; - Find dead filepaths, remove them from the database, and add new files to the database
; - Add commands to
;   - Reindex a file
;   - Reindex a directory
;   - Reindex all files in a database
;   - Reindex missing files
; With support for 
;   - Automigration
; Automigration:
;   - Find and compare similar file names, and automatically update filepath name

(defun main ()
  "Main application entry point"

; Define CLI & Parse Args
(build-cli)
(parse-opts (uiop:command-line-arguments))

; Show debug info
(if show-verbose (progn
        (if dir (format t "Using Directory: ~a~%" dir))
        (if db (format t "Using Database: ~a~%" db))
        (if pat (format t "With Globbing Pattern: ~a~%" pat))))

(opts:exit)

; Expands the directory path, and collects all the files
(defparameter file-dir (concatenate 'string (uiop:native-namestring dir) pat))
(defparameter files (directory file-dir))

; Connect and write to database
(connect db)
(ensure-tables (list 'document))
(add-to-db 'document files))
