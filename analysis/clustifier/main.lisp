(in-package :de.uni-saarland.syssec.clustifier)

(opts:define-opts 
#|  (:name :sql-query-hasher
	 :description "python script to hash a given sql query conforming our defined standards"
	 :short #\q
	 :long "sql-hasher"
	 :arg-parser #'identity)|#
  (:name :dump-folder 
	 :description "the folder the results shall be dumped into"
	 :short #\f
	 :long "dump-folder"
	 :arg-parser #'identity)
  (:name :database-path
	 :description "the full path to the database to be clustified"
	 :short #\d 
	 :long "database"
	 :arg-parser #'identity)
  (:name :start-id 
	 :description "the first id from which to extract http-request from the database"
	 :short #\s 
	 :long "start-id"
	 :arg-parser #'parse-integer)
  (:name :end-id
	 :description "the id up until which to extract http-requests from the database - this option is optional and defaults to 'up until the end'"
	 :short #\e
	 :long "end-id"
	 :arg-parser #'parse-integer)
  (:name :uselessness-threshold
	 :description "how many elements a cluster needs so that all contained requests are considered useless"
	 :short #\u 
	 :long "uselessness-threshold"
	 :arg-parser #'parse-integer))



(defun main ()
  (handler-case
      (multiple-value-bind (options free-args)
	  (opts:get-opts)
	(declare (ignore free-args))
	(if (or (not (getf options :database-path))
		(not (getf options :start-id)))
	    (error "missing parameters!")	     
	    (print:dump-results-into-folder
	     (database:get-all-requests (getf options :database-path) 
					(getf options :start-id)
					:end-id (if (getf options :end-id)
						    (getf options :end-id)
						    nil))					
	     (getf options :dump-folder)
	     (if (getf options :uselessness-threshold)
		 (getf options :uselessness-threshold)
		 2))))
    (error (err)
      (FORMAT T "~a~%" err)
      (opts:describe
       :prefix "This program is the badass doing all the clustering work to differentiate state changes of actions on webapplications - kneel before thy master"
       :suffix "so that's how it works…"
       :usage-of "run.sh"))))
