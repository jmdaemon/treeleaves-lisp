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
           :querydb
           :document
           :add-to-db
           ))
(in-package :treeleaves.models)

(require "mito")
(require "sxql")
(require "str")
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

(defun find-docs (&key query (order :desc))
  "Return a list of documents.
   If a query string is given, search on both the tags and the filepath fields.

   Usage:
   (find-docs :query \"Books\")
   "
  (mito:select-dao 'document
    (when (str:non-blank-string-p query)
      (sxql:where
       `(:and
         ,@(loop for word in (str:words query)
              :collect `(:or (:like :tags ,(str:concat "%" word "%"))
                             (:like :filepath ,(str:concat "%" word "%")))))))
       (sxql:order-by `(,order :created-at))))

; Tests
;(defparameter db "documents.sqlite")
;(defparameter tables (list 'document))
;(connect db)
;(ensure-tables tables)
;(find-docs :query "Books")

;(defun querydb (db tables query)

(defun showdocs (docs)
  (iterate (for doc in docs)
           (print (slot-value doc 'filepath)))
  (format t "~%"))

(defun query-db-all (db tables query)
  "Queries the database across all tables and for all document fields"
  (defparameter docs nil)
  (iterate (for search-term in query)
           (iterate (for table in tables)
                    (setq docs (find-docs table :query search-term))))
  
  (showdocs docs))

(defun querydb (db tables kword search-term)
  "Query the database and show matches

   Note that this function only queries for tag matches only "
  (connect db)
  (ensure-tables tables)
  (setq docs (find-doc (car tables) kword search-term))
  (showdocs docs))

; Define the document class
(mito:deftable document ()
                 ((tags :col-type (:varchar 4096))
                  (filepath :col-type (:varchar 4096))))

(defun add-to-db (db files)
  "Add new document entries to the database"
  (iter (for filepath in files)
        (if filepath
            (write-to-db db (format-tags (make-tag filepath)) (uiop:native-namestring filepath)))))

