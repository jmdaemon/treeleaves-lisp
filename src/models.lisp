(defpackage treeleaves.models
  (:use :cl)
  (:use :iter)
  (:documentation "Generate directory based file tags")
  (:import-from #:treeleaves.format
                #:fmt
                #:format-tags
                #:make-tag)
  (:export :connect
           :ensure-tables
           :write-to-db
           :find-doc
           :query
           :document
           :add-to-db
           ))
(in-package :treeleaves.models)

(require "mito")
(require "unix-opts")
(require "iterate")

;; Database Functions
(defun connect (dbname)
  "Connect to the database"
  (mito:connect-toplevel :sqlite3 :database-name dbname))

(defun ensure-tables (tables)
  "Creates database tables if not already existing"
  (mapcar #'mito:ensure-table-exists tables)) 

(defun write-to-db (table tags filepath)
  "Create and write table entries to the database"
  (mito:create-dao table :tags tags :filepath filepath))

(defgeneric find-doc (table key-name key-value)
  (:documentation "Retrieves a document from the data base by one of the unique
keys."))

(defmethod find-doc (table (key-name (eql :id)) (key-value integer))
  (mito:find-dao table key-value))

(defmethod find-doc (table key-name key-value)
  (mito:select-dao table (sxql:where (:like :tags key-value))))

(defun query (db tables free-args)
  "Query the database and show matches"
  (defparameter kword (fmt (car free-args)))
  (defparameter search-term (fmt (car (cdr free-args))))

  (connect db)
  (ensure-tables tables)
  (setq docs (find-doc (car tables) kword search-term))
  (iterate (for doc in docs)
        (print (slot-value doc 'filepath))
                )
  (format t "~%")
  (opts:exit))

; Define the document class
(mito:deftable document ()
                 ((tags :col-type (:varchar 4096))
                  (filepath :col-type (:varchar 4096))))

(defun add-to-db (db files)
  "Add new document entries to the database"
  (iter (for filepath in files)
        (if filepath
            (write-to-db db (format-tags (make-tag filepath)) (uiop:native-namestring filepath)))))

