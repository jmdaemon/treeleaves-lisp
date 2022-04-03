(defpackage treeleaves.cli
  (:use :cl :iter)
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

(require "unix-opts") 
(require "log4cl") 
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

(defun find-tables (str)
  "Finds and returns all tables found in str"
  (ppcre:register-groups-bind (matches)
                              ("([\\w ]*)" str :sharedp t)
                              (remove nil matches)))

;(find-tables "document -f ./documents.sqlite -q :tags Books %")

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


(defun parse-keyword (str)
    (ppcre:register-groups-bind (matches)
                                ("(:[\\w]*)" str :sharedp t)
                                matches))
;(parse-keyword ":tags")

(defun parse-search-args (str)
    (ppcre:register-groups-bind (*keyword* *search-term*)
                                ("(:[\\w]*) ([\\w %]*)" str :sharedp t)
                                (list *keyword* *search-term*)))

; Test
;(parse-keyword "document -f ./documents.sqlite -qa :tags Books %")
;(parse-search-term ":tags Books %")
;(parse-search-term "document -f ./documents.sqlite -qa :tags Books %")

(defun format-args (argv) 
  (format NIL "~{~a~^ ~}" argv))

(defun parse-search (free-args)
  (defparameter args (format-args (opts:argv)))
  (parse-search-args args))

;(parse-search "document -f ./documents.sqlite -qa :tags Books %")

;(parse-search (list "document -f ./documents.sqlite -qa :tags Books %"))
;(parse-search-args "document -f ./documents.sqlite -qa :tags Books %")
;(parse-search "document -f ./documents.sqlite -qa :tags Books %")

; # Test destructuring
;(defparameter args (list "document" "./documents.sqlite" "-qa" ":tags" "Books %"))
;(defparameter stringargs (format-args args))
;(parse-search stringargs)
;(destructuring-bind (*keyword* *search-term*) (parse-search stringargs)
  ;(list :kword *keyword* :search-term *search-term*))
;(destructuring-bind (*keyword* *search-term*) (parse-search stringargs))

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
            (log:info "In options :tables")
            (defparameter args (opts:argv))
            (log:info "Args: " args)

            (defparameter argv (cdr args))
            (log:info "Argv: " argv)

            (defparameter argstr (format-args argv))
            (log:info "Argstr: " argstr)

            (setq tables (list 'document))
            (log:info "Tables: " tables) 
            ))

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
           (defparameter kword nil)
           (defparameter search-term nil)
           (destructuring-bind (*keyword* *search-term*) (parse-search free-args)
             ;(list kword *keyword* search-term *search-term*)
             (setq kword *keyword*)
             (setq search-term *search-term*))
           (log:info "Keyword: " kword)
           (log:info "Search-Term: " search-term)

           (initdb db tables)
           (log:info "Initialized Tables")

           (querydb tables kword search-term)
           (log:info "Search Complete")

           (opts:exit)))

      (if (getf options :qa)
          (progn
            (initdb db tables)
            (querydb-all tables (parse-search free-args))
            (opts:exit)))
      ))
