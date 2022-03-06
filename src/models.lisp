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
;(require "sxql")
(require "iterate")
;(use-package :iter)
;(use-package :iterate)
;(use-package :iter)
;(require "iterate")
;(use-package :iter)
;(require "str")

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

;"Matches like tags"
;(defmethod find-doc (table (key-name (eql :tags)) (key-value string))
  ;(first (mito:select-dao table
                          ;(sxql:where (:like :tags key-value)))))  

(defmethod find-doc (table key-name key-value)
  (mito:select-dao table (sxql:where (:like :tags key-value))))
  ;(first (mito:select-dao table
                          ;(sxql:where (:like :tags key-value)))))  

; Custom SXQL Query
;(defun find-files (&key query (order :desc))
  ;"Return a list of files. If a query string is given, search on both the title and the author fields."
  ;(mito:select-dao 'document
    ;(when (str:non-blank-string-p query)
    ;;(when query
      ;(sxql:where
       ;`(:and
         ;,@(loop for word in (str:words query)
              ;:collect `(:or (:like :tags ,(str:concat "%" word "%"))
                             ;(:like :filepath ,(str:concat "%" word "%")))))))
       ;(sxql:order-by `(,order :created-at))))
;(find-files )

;(iterate:iter (for i in '(1 2 3 4 5))
              ;(print i))


(defun query (db tables free-args)
  ;(print free-args)
  ;(type-of free-args)
  (defparameter kword (fmt (car free-args)))
  (defparameter search-term (fmt (car (cdr free-args))))

  (connect db)
  (ensure-tables tables)
  ;(print (find-doc (car tables) kword search-term))
  (setq docs (find-doc (car tables) kword search-term))
  ;(iterate:iter (for doc in docs)
  (iterate (for doc in docs)
        (print (slot-value doc 'filepath))
        ;(collect (slot-value doc 'filepath))
                ;(print (mito:filepath doc))
                )
  ;(print )
  (format t "~%")
  (opts:exit))
  ;)

;(query "documents.sqlite" (list 'document) (list :tags "Books %"))
;(find-doc 'document :tags "Books %")
;(setq doc (first (mito:select-dao 'document
                      ;(sxql:where (:like :tags "Books %")))))
;(type-of doc)
;(access doc :file-path)
;(getf (slot-value doc 'filepath))
;(slot-value doc 'filepath)

;(require "closer-mop")

;(mito.class.column:table-column-class)

;(closer-mop:class-direct-slots (find-class 'document))
; Define the document class
(mito:deftable document ()
                 ((tags :col-type (:varchar 4096))
                  (filepath :col-type (:varchar 4096))))
;(mito:deftable document ()
                 ;((tags :col-type (:varchar 4096)
                        ;:initarg :tags
                        ;:accessor file-tags)
                  ;(filepath :col-type (:varchar 4096)
                        ;:initarg :filepath
                        ;:accessor file-path)))
(defun add-to-db (db files)
  (loop for filepath in files
        do
        (if filepath
            (write-to-db db (format-tags (make-tag filepath)) (uiop:native-namestring filepath)))))

