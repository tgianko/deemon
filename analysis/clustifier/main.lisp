(in-package :de.uni-saarland.syssec.clustifier)

(opts:define-opts 
  (:name :sql-query-hasher
	 :description "python script to hash a given sql query conforming our defined standards"
	 :short #\q
	 :long "sql-hasher"
	 :arg-parser #'identity)
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
	 :description "the id up until which to extract http-requests from the database"
	 :short #\e
	 :long "end-id"
	 :arg-parser #'parse-integer))



(defun main ()
  (handler-case
      (multiple-value-bind (options free-args)
	  (opts:get-opts)
	(declare (ignore free-args))
	(print:dump-results-into-folder
	 (database:get-all-requests (getf options :database-path) 
				    (getf options :start-id)
				    :end-id (if (getf options :end-id)
						(getf options :end-id)
						nil))
	 (getf options :dump-folder)))
    (unix-opts:unknown-option (err)
      (declare (ignore err))
      (opts:describe
       :prefix "This program is the badass doing all the clustering work to differentiate state changes of actions on webapplications - kneel before thy master"
       :suffix "so that's how it works…"
       :usage-of "run.sh"))))


(print:dump-results-into-folder
 (database:get-all-requests "/home/simkoc/.vilanoo/abantecart/abantecard-create-account-201607221205.db"
			    "0"
			    :end-id nil)
 "/home/simkoc/tmp/vilanoo-abantecard-create-account/")

(print:dump-results-into-folder
 (database:get-all-requests "/home/simkoc/.vilanoo/abantecart/abantecard-change-mail-201607221211.db"
			    "0"
			    :end-id nil)
 "/home/simkoc/tmp/vilanoo-abantecard-change-email/")


(print:dump-results-into-folder
 (database:get-all-requests "/home/simkoc/.vilanoo/abantecart/abantecard-change-password-201607221348.db"
			    "0"
			    :end-id nil)
 "/home/simkoc/tmp/vilanoo-abantecard-change-pwd/")


(print:dump-results-into-folder
 (database:get-all-requests "/home/simkoc/.vilanoo/opencart/opencart-create-account-201607221357.db"
			    "0"
			    :end-id nil)
 "/home/simkoc/tmp/vilanoo-opencart-create-account/")



(print:dump-results-into-folder
 (database:get-all-requests "/home/simkoc/.vilanoo/opencart/opencart-change-password-201607221359.db"
			    "0"
			    :end-id nil)
 "/home/simkoc/tmp/vilanoo-opencart-change-password/")



(print:dump-results-into-folder
 (database:get-all-requests "/home/simkoc/.vilanoo/opencart/opencart-change-email-201607221408.db"
			    "0"
			    :end-id nil)
 "/home/simkoc/tmp/vilanoo-opencart-change-email/")

(sb-thread:destroy-thread (cadr (sb-thread:list-all-threads)))
