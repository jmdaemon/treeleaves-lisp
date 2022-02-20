(defpackage :treeleaves
  (:use :cl)
  (:documentation "Generate directory based file tags")
  (:export :main)
  )
(in-package :treeleaves)

; Imports
(require "uiop") 
(require "cl-utilities")
(require "mito")
(require "unix-opts")

; Main application entry point
(defun main ()

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
       :long "pattern")
    )

(defun show-usage ()
  (progn
    (opts:describe
      :prefix "Generate directory based file tags"
      :args "[keywords]")
    (opts:exit)))

; Parse CLI Opts
(multiple-value-bind (options free-args)
                   (opts:get-opts (uiop:command-line-arguments))
    ; The directory to the pdf files
    (if (getf options :help)
        (show-usage))
    (if (getf options :fp)
        (defparameter dir (format t "~a~&" free-args))
        (defparameter dir "~/Documents")
        )
    (if (getf options :o)
        (defparameter db (format t "~a~&" free-args))
        (defparameter db "documents.sqlite"))
    (if (getf options :p)
        (defparameter pat (format t "~a~&" free-args))
        (defparameter pat "/**/*.pdf"))
    )

(if (eql dir "~/Documents")
    (format t "Using default directory: ~a~%" dir))

(if (eql db "documents.sqlite")
    (format t "Using default database name: ~a~%" db))

(if (eql pat "/**/*.pdf")
    (format t "Globbing pattern: ~a~%" pat))

; Expands the directory path, and collects all the pdf files
(defparameter pdf-dir (concatenate 'string (uiop:native-namestring dir) pat))
(defparameter pdf-files (directory pdf-dir))

;Splits a directory file path on '/' characters
(defun split-dir (*dir*)
    (cl-utilities:split-sequence #\/ *dir*))

(defun make-tag (filepath)
  (defparameter split-filepath (split-dir (uiop:native-namestring filepath)))
  (subseq split-filepath 4))

(defun print-tags (tags) 
  (format t "~{~a~^ ~}~%" tags))

(defun format-tags (tags) 
  (format NIL "~{~a~^ ~}~%" tags))

; Connect to SQLite3 database, initializes it if it doesn't exist
(mito:connect-toplevel :sqlite3 :database-name db)

; Define a new entry
(mito:deftable document ()
  ((tags :col-type (:varchar 4096))
   (filepath :col-type (:varchar 4096))))

; Create the table
(mito:ensure-table-exists 'document)

; Create entries and write them to the database
(defun write-to-db (tags filepath)
  (mito:create-dao 'document :tags tags :filepath filepath))

; Find documents in the data base
(defgeneric find-doc (key-name key-value)
  (:documentation "Retrieves a document from the data base by one of the unique
keys."))

(defmethod find-doc ((key-name (eql :id)) (key-value integer))
  (mito:find-dao 'document key-value))

; Matches like tags
(defmethod find-doc ((key-name (eql :tags)) (key-value string))
  (first (mito:select-dao 'document
                          (sxql:where (:like :tags key-value)))))

; Adds all the documents with their tags and filepath
(loop for filepath in pdf-files
    do
    (if filepath
        (write-to-db (format-tags (make-tag filepath)) (uiop:native-namestring filepath))
        (print "Done"))))

; Make sure to add % when matching like terms
;(find-doc :tags "Books %"))
