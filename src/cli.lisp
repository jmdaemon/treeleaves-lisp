(defpackage treeleaves.cli
  (:use :cl)
  (:documentation "Create the CLI & Parse CLI arguments")
  (:import-from #:treeleaves.format
                #:fmt
                )
  (:import-from #:treeleaves.models
                #:querydb
                #:document
                )
  (:export :argparse
           :build-cli
           :show-usage
           :show-verbose
           :parse-opts
           ))
(in-package :treeleaves.cli)

(require "unix-opts") 
(require "uiop") 

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
    (:name :db
           :description "Query the database for files"
           :short #\f
           :long "database")
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
            (querydb db (list 'document) (parse-query free-args))
            (opts:exit)))))
