(defpackage treeleaves.cli
  (:use :cl :iter)
  (:documentation "Create the CLI & Parse CLI arguments")
  (:import-from #:treeleaves.format
                #:fmt
                #:format-args
                #:format-tags
                #:make-tag
                #:find-tables
                #:parse-tables
                #:parse-search-args
                #:parse-database-args
                )
  (:import-from #:treeleaves.models
                #:*database-table-types*
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
(require "cl-utilities")
(require "log4cl") 
(require "uiop") 
(require "str")
(require "iterate")

; Globals

; https://github.com/7max/log4cl#category
; Log levels available:
; 0: OFF, 1: FATAL, 2: ERROR, 3: WARN, 4: INFO, 5:DEBUG
; Sets the log level to LOG_LEVEL_CL if set
(defparameter loglevel (uiop:getenv "LOG_LEVEL_CL"))
(if loglevel
 (log:config :pretty loglevel); Set the log level
 (log:config :pretty :error)) ; Else only show errors

; Variables
; Show verbose information
(defparameter show-verbose nil)
(defparameter tables nil)
(defparameter db nil)
(defparameter dir nil)
(defparameter pat nil)

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
    (:name :gen-tags
           :description "Generate and print the corresponding tags only"
           :short #\g
           :long "--generate")
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

(defun string-to-table (*key*)
  "Returns the database table corresponding to a string key"
  (defparameter treeleaves-table nil)
  ; Check to see if that table type exists in our database
  (setq treeleaves-table (gethash *key* *database-table-types*))
  ; If no table types were found
  (if (equal nil treeleaves-table)
      (progn ; Error out
        (format t "No tables with name ~A were found.~%" *key*)
        (opts:exit)))
  treeleaves-table
  )

; Test
;(string-to-table "document")

(defun parse-opts (args)
  "Parses our command line options"
  (multiple-value-bind (options free-args)
    (opts:get-opts args)
      ; Show help message
      (if (getf options :help)
          (show-usage))

      ; Set verbose output
      (if (getf options :verbose)
          (setq show-verbose t)
          (setq show-verbose nil))

      ;; Querying the database
      ; Set the database file path
      (if (getf options :db)
          (progn
            (defparameter argstr (format-args (opts:argv)))
            (log:info "Argstr: " argstr)
            (defparameter dbname (parse-database-args argstr))
            (log:info "Database Name: " dbname)
            (setq db (uiop:native-namestring dbname))
            (log:info "In options :db")
            (log:info "Set db to " db)
            (if (equal db nil)
                (progn
                  (log:info "Could not set database path")
                  (opts:exit))))
          (setq db (uiop:native-namestring "./documents.sqlite")))

      ; Set the tables to lookup in the database
      (if (getf options :tables)
          (progn
            (log:info "In options :tables")
            (defparameter argstr (format-args (opts:argv)))
            (log:info "Argstr: " argstr)

            (defparameter table-keyword (find-tables argstr))
            (log:info "Table-Keyword: " table-keyword)

            (defparameter table-trimmed (str:trim-right table-keyword))
            (log:info "Result from find-tables: " table-trimmed)

            (defparameter treeleaves-table (string-to-table table-trimmed))

            (setq tables (list treeleaves-table))
            (log:info "Tables: " tables))
          (setq tables (list 'document)))

      ;; Generating the database
      ; Sets the root file path to discover files
      (if (getf options :fp)
          (setq dir (format-args free-args))
          (setq dir "~/Documents"))

      ; Generates the database with this file path/name
      (if (getf options :o)
          (setq db (uiop:native-namestring (parse-database-args (format-args free-args))))
          (setq db db))

      ; The file globbing pattern to be used to discover files
      (if (getf options :p)
          (setq pat (format-args free-args))
          (setq pat "/**/*.pdf"))

      ; Generate and print the tags only
      (if (getf options :gen-tags)
          (progn
            ; Collect the names of files in the directory
            (defparameter file-dir (concatenate 'string (uiop:native-namestring dir) pat))
            (defparameter files (directory file-dir))
            ; Output the generated tags
            (iter (for filepath in files)
                  (defparameter tags (format nil "~{~a~^ ~}" (make-tag filepath)))
                  (format t "~A~A~%" tags filepath)
                )))

      ;; Database queries
      (if (getf options :q)
          (progn
            (log:info "Database Path: " db)
            (log:info "Database Table: " tables)

            (defparameter kword nil)
            (defparameter search-term nil)
            (destructuring-bind (*keyword* *search-term*) (parse-search free-args)
              (setq kword *keyword*)
              (setq search-term *search-term*))
            (log:info "Keyword: " kword)
            (log:info "Search-Term: " search-term)

            (initdb db tables)
            (log:info "Initialized Database")

            (querydb tables kword search-term)
            (log:info "Search Complete")
            (opts:exit)))

      (if (getf options :qa)
          (progn
            (log:info "Database Path: " db)
            (log:info "Database Table: " tables)

            (initdb db tables)
            (log:info "Initialized Database")

            (querydb-all tables (parse-search free-args))
            (log:info "Search Complete")
            (opts:exit)))
      ))
