(defpackage treeleaves.cli
  ;(:use :cl :treeleaves.models)
  (:use :cl :iter :treeleaves.models)
  (:documentation "Create the CLI & Parse CLI arguments")
  (:import-from #:treeleaves.format
                #:fmt
                )
  (:import-from #:treeleaves.models
                #:document
                #:initdb
                #:querydb
                ) (:export :argparse
           :build-cli
           :show-usage
           :show-verbose
           :parse-opts
           ))
(in-package :treeleaves.cli)

;(let (symbols)
  ;(do-external-symbols (s (find-package :treeleaves.models))
    ;(push s symbols))
  ;(print symbols))
;(print symbols)

(require "unix-opts") 
(require "cl-ppcre")
(require "cl-utilities")
(require "uiop") 
(require "str") 
(require "iterate") 
(require "mito")

(defparameter show-verbose nil)

(defun argparse (free-arg)
  "Parses a CLI argument"
  (format t "~a~&" free-arg)) 

(defun build-cli ()
  "Builds the CLI interface"
  (opts:define-opts
    (:name :help
           :description "Show this help message"
           :short #\h
           :long "help")
    (:name :verbose
           :description "Show verbose information"
           :short #\v
           :long "verbose")
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
           :long "query")
    (:name :qa
           :description "Query the database for files across all fields"
           :short #\a
           :long "query-all")
    (:name :db
           :description "Use an existing database"
           :short #\f
           :long "database")
    (:name :tables
           :description "Use an existing database table"
           :short #\t
           :long "tables")
    ))

(defun show-usage ()
  "Shows a message describing how to use the program"
  (progn
    (opts:describe
      :prefix "Generate directory based file tags"
      :args "[keywords]")
    (opts:exit)))

(defun parse-query (free-args)
  "Parses the cli args for a database query"
  ; This should be modified to allow queries on multiple kwords and search-terms
  (if (eq (length free-args) 3)
      (progn
        (defparameter kword (fmt (second free-args)))
        (defparameter search-term (fmt (third free-args))))
      (progn
        (defparameter kword (first free-args))
        (defparameter search-term (second free-args))))
  (list kword search-term))

;(defmacro table (tbl) `(read-from-string tbl))
;(declaim (ftype 'document))
;(import 'treeleaves.models:document)

(defun find-tables (str)
  "Finds and returns all tables found in str"
  (ppcre:register-groups-bind (matches)
                              ;("-t (\w)* (-)?" arg :sharedp t)
                              ;("-t ([\\w ]*)" str :sharedp t)
                              ("([\\w ]*)" str :sharedp t)
                              ;(list kword))
                              ;matches)
                              ;(remove nil (str:trim-right matches))))
                              (remove nil matches)))

;(find-tables "document -f ./documents.sqlite -q :tags Books %")

;(mito:deftable document ()
                 ;((tags :col-type (:varchar 4096))
                  ;(filepath :col-type (:varchar 4096))))

;(defun find-table (tablestr)
  ;(defparameter tablesymb (read-from-string tablestr))
  ;;(if (not (equal (find-class tablesymb) nil))
  ;(if (find-class tablesymb nil)
      ;(class-name (find-class tablesymb))))

(defun interpret-string-as-dbtable (tables-stringlist)
  (defparameter tables
    (mapcar #'(lambda (tablestr)
                (progn
                  (defparameter tablesymb (read-from-string tablestr))
                  (if (find-class tablesymb nil)
                      `,tablesymb))) tables-stringlist))
  tables
  )

;(find-class 'document)

(defun get-symbols (pkg)
  (defparameter symbols nil)
  (do-external-symbols (s (find-package pkg))
    (push s symbols))
  symbols
      ;(push s symbols)
  ;(defparameter symbols nil)
  ;(let (symbols)
    ;(do-external-symbols (s (find-package pkg))
      ;(push s symbols))
    ;symbols)
  ;symbols
  )

(defun parse-tables (args)
  "Returns a list of the database tables found in free-args"

  ; Make sure free-args is converted to a string
  ;(defparameter argstring free-args)
  (defparameter tables-string (str:trim-right (find-tables args)))
  (print "Result from find-tables:")
  (print tables-string)
  (defparameter tables-stringlist (cl-utilities:split-sequence #\Space tables-string))
  (print "Result from split")
  (print tables-stringlist)
  ;(defparameter tables (interpret-string-as-dbtable tables-stringlist))
  ;(defparameter tables (list `,(read-from-string (car tables-stringlist))))
  ;(defparameter tables (list `,(find-class (read-from-string (car tables-stringlist)))))
  ;(defparameter tables (list `,(class-name (find-class (read-from-string (car tables-stringlist))))))
  ;(defparameter tables (list (read-from-string (car tables-stringlist))))

  ;(defparameter tables (list `,(class-name (find-class (read-from-string (car tables-stringlist))))))
  ;(defparameter tables (list `,(read-from-string "document")))
  ;(unintern 'document)
  ;(setf (find-class `,(read-from-string "document")) mito:dao-table-class)
  ;(defparameter tables (list (setf (find-class `,(read-from-string "document")) #'mito:dao-table-class)))
  ;(defparameter tables (list (setf (find-class `,(read-from-string "document")) #'mito:dao-table-class)))

  ;(defparameter tables (list (class-name (find-class (read-from-string "document")))))
  ;(setf tables (list (make-instance `,(read-from-string "document"))))
  ;(setf tables (list (read-from-string "document")))
  ;(defparameter tables (list (setf (find-class `,(read-from-string "document")) 'document)))
  ;(defparameter tables (list (setf (find-class `,(read-from-string "document")) `,:metaclass mito:dao-table-class)))
  (defparameter tables (list (find-class 'document)))
  ;(setf classes (list (read-from-string "document")))
  ;(defparameter symbols (get-symbols :treeleaves.models))
  ;(defparameter tables nil)
  ;(iterate (for *class* in classes)
           ;(iterate (for *symbol* in symbols)
                    ;;(if (assoc *symbol* *class*)
                    ;(if (eq *symbol* *class*)
                        ;(push *class* tables)
                        ;;(setq tables (append tables (list `,*class*)))
                        ;)))
;(let (symbols)
  ;(do-external-symbols (s (find-package :treeleaves.models))
    ;(push s symbols))
  ;(print symbols))

  
  (print "Result from tables")
  (print tables)
  ;(print `,tables)
  (remove nil tables))


; Test 
;(defparameter db "documents.sqlite")
;(defparameter tbls (parse-tables "-t document books"))
;tbls
;(initdb db tbls)
;(querydb tbls ":tags" "Books %")
;(initdb "documents.sqlite" (list 'document))
;;(querydb (list 'document) ":tags" "Books %")


  ;(defparameter tables (mapcar (lambda (tablestr) 
            ;(progn
             ;(defparameter tablesymb (read-from-string tablestr))
             ;;(if (not (equal (find-class tablesymb) nil))
             ;(if (find-class tablesymb nil)
                 ;(find-class tablesymb))
             ;)) tables-stringlist))


  ;(defparameter tables (mapcar (lambda (tablestr) (find-table tablestr)) tables-stringlist))

  ;tables)
  ;(mapcar (lambda (tablestr) (read-from-string tablestr)) tables-stringlist))
  ;(mapcar (lambda (tablestr) (macroexpand table tablestr)) tables-stringlist))
  ;(print (mapcar (lambda (tablestr) (read-from-string tablestr)) tables-stringlist))
  ;(opts:exit))

;(defparameter argstring "-t document books -f thing")
;(defparameter tables-string (str:trim-right (find-tables argstring)))

;(defparameter tables-stringlist (cl-utilities:split-sequence #\Space tables-string))
;(mapcar list (read-from-string)) tables-stringlist
;(mapcar (lambda (tablestr) (read-from-string tablestr)) tables-stringlist)
;(./documents.sqlite document :tags Books %)

;tbls
;(initdb "documents.sqlite" tbls)



;(list 'document)
;(setq tbls (parse-tables "document books"))
;(setq tbls (parse-tables "-t document"))
;tbls
;(setq tbl (car tbls))
;(type-of tbl)




;(opts:get-opts (list "-f" "documents.sqlite" "-t" ""))

;(defparameter tablestr (find-tables "-t document books -f thing"))
;(str:trim-right tablestr)
;(defparameter tableset (cl-utilities:split-sequence #\Space tablestr))
;(defparameter table (first tableset))
;'table
;'document

;(initdb db (list (read-from-string table)))

;(cdr (list "./bin/treeleaves" "-t" "document book"))

(defun parse-keyword (str)
    (ppcre:register-groups-bind (matches)
                                ("(:[\\w]*)" str :sharedp t)
                                matches))
;(parse-keyword ":tags")
;(defun parse-search-term (str)
    ;(ppcre:register-groups-bind (matches)
                                ;(" ([\\w %]*)" str :sharedp t)
                                ;matches))
;(defun parse-search-term (str)
(defun parse-search-args (str)
    (ppcre:register-groups-bind (*keyword* *search-term*)
                                ("(:[\\w]*) ([\\w %]*)" str :sharedp t)
                                (list *keyword* *search-term*)))
;(parse-search-term ":tags Books %")

;(parse-keyword "document -f ./documents.sqlite -qa :tags Books %")
;(parse-search-term "document -f ./documents.sqlite -qa :tags Books %")
;(parse-search "document -f ./documents.sqlite -qa :tags Books %")

;(multiple-value-bind (kword search-term)
  ;(parse-search-term "document -f ./documents.sqlite -qa :tags Books %"))
;kword
;search-term

(defun format-args (argv) 
  (format NIL "~{~a~^ ~}" argv))

(defun parse-search (free-args)
  ;(defparameter args (argparse free-args))
  ;(defparameter args (argparse free-args))
  ;(defparameter *keyword* (parse-keyword args))
  ;(defparameter *search-term* (parse-search-term args))
  ;(list *keyword* *search-term*)
  ;(parse-search-args args)
  (parse-search-args free-args))

;(parse-search (list "document -f ./documents.sqlite -qa :tags Books %"))
;(parse-search-args "document -f ./documents.sqlite -qa :tags Books %")
;(parse-search "document -f ./documents.sqlite -qa :tags Books %")
;(defparameter args (list "document" "./documents.sqlite" "-qa" ":tags" "Books %"))
;(defparameter stringargs (format-args args))
;(parse-search stringargs)

(defparameter tables 'document)

(defun parse-opts (args)
  "Parses our command line options"
  (multiple-value-bind (options free-args)
    (opts:get-opts args)
      (if (getf options :help)
          (show-usage))

      (if (getf options :verbose)
          (setq show-verbose t)
          (setq show-verbose nil))

      (if (getf options :db)
          (defparameter db (argparse free-args))
          (defparameter db "documents.sqlite"))

      (if (getf options :tables)
          (progn
            (defparameter args (opts:argv))
            (print args)

            (defparameter argv (cdr args))
            (print argv)

            (defparameter argstr (format-args argv))
            (print argstr)

            ;(setq tables (parse-tables argstr))
            ;(defparameter tables (parse-tables argstr))
            ;(setq tables (parse-tables argstr))
            (setq tables (list 'document))
            (print tables) 
            ;(print (type-of tables))
            ;(if (equal tables nil)
                ;(opts:exit))
            )
          ;(defparameter tables (list 'document))
          )

      (if (getf options :fp)
          (defparameter dir (argparse free-args))
          (defparameter dir "~/Documents"))

      (if (getf options :o)
          (defparameter db (argparse free-args))
          (defparameter db "documents.sqlite"))

      (if (getf options :p)
          (defparameter pat (argparse free-args))
          (defparameter pat "/**/*.pdf"))

      (if (getf options :q)
          (progn
           ;(multiple-value-bind (kword search-term) (parse-query free-args))
           ;(defparameter argv (argparse (cdr (format-args opts:argv))))
           ;(defparameter argv (argparse (cdr (format-args (opts:argv)))))
           (defparameter argv (format-args (opts:argv)))
           ;(multiple-value-bind (kword search-term) (parse-search argv))
           (multiple-value-bind (parsed) (parse-search argv)
               ;(defparameter args (parse-search argv))
               (print "Parsed Args")
               (print parsed)
               (defparameter kword (first parsed))
               (defparameter search-term (second parsed))
               (print "Keyword, Search-Term")
               (print kword)
               (print search-term)
               (initdb db tables)
               (print "Initialized Tables")
               (querydb tables kword search-term)
               (print "Search Complete")
               (opts:exit)
               
               )))
          ;(progn
            ;(if tables
                ;(progn
                  ;(multiple-value-bind (kword search-term) (parse-query free-args))
                  ;;(print kword search-term)
                  ;(initdb db tables)
                  ;(querydb tables kword search-term)
                  ;(opts:exit)
                  ;))))

      (if (getf options :qa)
          (progn
            (initdb db tables)
            (querydb-all tables (parse-query free-args))
            (opts:exit)))
      ))
