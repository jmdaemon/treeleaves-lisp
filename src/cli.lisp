(defpackage treeleaves.cli
  (:use :cl :iter)
  (:documentation "Create the CLI & Parse CLI arguments")
  (:import-from #:treeleaves.format
                #:fmt
                #:format-args
                #:parse-tables
                #:parse-search-args
                )
  (:import-from #:treeleaves.models
                #:document
                #:initdb
                #:querydb
                ) 
  (:export :build-cli
           :show-usage
           :show-verbose
           :parse-opts
           ))
(in-package :treeleaves.cli)

(require "unix-opts") 
(require "log4cl") 
(require "uiop") 

; Globals

; Sets the log level to LOG_LEVEL_CL if set
; Otherwise defaults to no logging
(defparameter loglevel (uiop:getenv "LOG_LEVEL_CL"))
(if loglevel
 (log:config :pretty loglevel)
 (log:config :pretty :warn))

; Show verbose information
(defparameter show-verbose nil)
(defparameter tables 'document)

;; TODO:
;; Write up a macro that generates this code
;; with an &optional body to specify further options
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

(defun parse-search (free-args)
  "Retrieves the keyword and search terms for a database query"
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

(defun parse-opts (args)
  "Parses our command line options"
  (multiple-value-bind (options free-args)
    (opts:get-opts args)
      (if (getf options :help)
          (show-usage))

      ; Set verbose output
      (if (getf options :verbose)
          (setq show-verbose t)
          (setq show-verbose nil))

      ;; Querying the database
      ; Sets the database file path
      (if (getf options :db)
          (defparameter db (format-args free-args))
          (defparameter db "documents.sqlite"))

      ; Set the tables to lookup in the database
      (if (getf options :tables)
          (progn
            (log:info "In options :tables")
            (defparameter argstr (format-args (opts:argv)))
            (log:info "Argstr: " argstr)
            (setq tables (list 'document))
            (log:info "Tables: " tables) 
            ))

      ;; Generating the database
      ; Sets the root file path to discover files
      (if (getf options :fp)
          (defparameter dir (format-args free-args))
          (defparameter dir "~/Documents"))

      ; Generates the database with this file path/name
      (if (getf options :o)
          (defparameter db (format-args free-args))
          (defparameter db "documents.sqlite"))

      ; The file globbing pattern to be used to discover files
      (if (getf options :p)
          (defparameter pat (format-args free-args))
          (defparameter pat "/**/*.pdf"))

      ;; Database queries
      (if (getf options :q)
          (progn
           (defparameter kword nil)
           (defparameter search-term nil)
           (destructuring-bind (*keyword* *search-term*) (parse-search free-args)
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
