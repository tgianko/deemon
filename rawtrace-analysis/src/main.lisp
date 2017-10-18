; This file is part of Deemon.

; Deemon is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; Deemon is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with Deemon.  If not, see <http://www.gnu.org/licenses/>.

(in-package :de.uni-saarland.syssec.analyzer)


(defparameter *php-session-diff-state* nil)


(defparameter *file-diff-state* nil) 


(opts:define-opts
  (:name :source-database-mosgi
	 :description "the database path of mosgis db from which to retrieve the information to analyze"
	 :short #\m
	 :long "source-database-mosgi"
	 :arg-parser #'identity)
  (:name :source-database-vilanoo
	 :description "the database path of vilanoos db from which to retrieve the information to analyze"
	 :short #\v
	 :long "source-database-vilanoo"
	 :arg-parser #'identity)
  (:name :sink-database
	 :description "the database path into which to write the analyzed information"
	 :short #\d
	 :long "sink-database"
	 :arg-parser #'identity)
  (:name :sink-database-schema
	 :description "the schema for the sink database schema"
	 :short #\S 
	 :long "sink-schema"
	 :arg-parser #'identity)
  (:name :start-id 
	 :description "the id from which to start the analysis of the http requests"
	 :short #\f 
	 :long "start-id"
	 :arg-parser #'parse-integer)
  (:name :end-id 
	 :description "the last id to process while analyzing the http requests"
	 :short #\e 
	 :long "end-id"
	 :arg-parser #'parse-integer)
  (:name :keep-all-queries-p
         :description "all SQL queries are kept and non state-changing ones are not removed (y/N)"
         :short #\k 
         :long "keep-all-queries"
         :arg-parser #'(lambda (string)
                         (if (or (string= string "y")
                                 (string= string "Y"))
                             T
                             NIL))))

                             
(defun make-diff (id-list db-source-connection db-sink-connection keep-all-queries-p)
  (database:copy-http-request-entries id-list db-source-connection db-sink-connection)
  (let ((*file-diff-state* (make-instance 'analysis:state-trace))
	(*php-session-diff-state* (make-instance 'analysis:state-trace)))
    (do ((rem-ids id-list (cdr rem-ids)))
	((not rem-ids) nil)
      (FORMAT T "php session analysis for request ~a/~a~%" (car rem-ids) (car (last rem-ids)))
      (database:commit-raw-sessions 
       (car rem-ids)
       (database:get-all-session-entries (car rem-ids) db-source-connection)
       db-sink-connection) 
      (FORMAT T "xdebug analysis for request ~a/~a~%" (car rem-ids) (car (last rem-ids)))
      (let ((xdebug (xdebug:make-xdebug-trace-from-file (database:get-xdebug-entry-as-file-path (car rem-ids) db-source-connection))))
	(analysis:add-next-state-* *file-diff-state* 
				   (analysis:make-file-history-state 
				    (xdebug:get-changed-files-paths 
				     xdebug)))
	(database:commit-sql-queries db-sink-connection (car rem-ids) (xdebug:get-sql-queries xdebug keep-all-queries-p))
	(database:commit-latest-diff db-sink-connection (car rem-ids) *file-diff-state*)))))


(defmacro aif (condition then &optional else)
  `(let ((it ,condition))
     (if it
	 ,then
	 ,else)))


(defun main ()
  (handler-case 
      (multiple-value-bind (options free-args)
	  (opts:get-opts)
	(declare (ignore free-args))
	(let ((source-database-path-vilanoo (aif (getf options :source-database-vilanoo) it (error "source database path vilanoo has to be provided")))
	      (source-database-path-mosgi (aif (getf options :source-database-mosgi) it (error "source database path mosgi has to be provided")))
	      (sink-database-path (aif (getf options :sink-database) it (error "sink database path has to be provided")))
	      (sink-database-schema (aif (getf options :sink-database-schema) it (error "sink database schema has to be provided")))
	      (start-id (aif (getf options :start-id) it 0))
              (keep-all-queries-p (aif (getf options :keep-all-queries-p) it NIL)))
	  (FORMAT T "~a~%~a~%~a~%~a~%" source-database-path-mosgi source-database-path-vilanoo sink-database-path sink-database-schema)
	  (clsql:with-database (source-db-mosgi (list source-database-path-mosgi) :database-type :sqlite3)
	    (clsql:with-database (sink-db (list sink-database-path) :database-type :sqlite3)
	      (database:create-database sink-db sink-database-schema)
	      (clsql:with-database (source-db-vilanoo (list source-database-path-vilanoo) :database-type :sqlite3) ;;this is needed as vilanoo and mosgi use split db due to 
		(database:merge-databases source-db-vilanoo source-db-mosgi)) ;;datarace problems of sqlite3
	      (let ((end-id (aif (getf options :end-id) it (+ (database:get-highest-http-request-id-entry source-db-mosgi) 1))))	    
		(make-diff (remove-if #'(lambda(id)
                                          (or (< id start-id) 
                                              (> id end-id)))
                                      (database:get-all-http-request-ids source-db-mosgi)) source-db-mosgi sink-db keep-all-queries-p))))))
  (unix-opts:unknown-option (err)
    (declare (ignore err))
    (opts:describe
     :prefix "This program extracts the relevant data previously acquired from mosgi during rawtrace acquisition"
     :suffix "so that's how it works…"
     :usage-of "run-analyzer.sh"))))
