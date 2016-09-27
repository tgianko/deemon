(defpackage de.uni-saarland.syssec.analyzer
  (:use :cl)
  (:nicknames :analyzer)
  (:export main))


(defpackage de.uni-saarland.syssec.analyzer.database
  (:use :cl)
  (:nicknames :database)
  (:export create-database
	   copy-http-request-entries
	   get-highest-http-request-id-entry
	   get-all-http-request-ids
	   get-all-session-entries
	   get-xdebug-entry
           commit-sql-queries
	   commit-latest-diff
	   commit-full-sessions
	   commit-latest-diff
	   merge-databases))


(defpackage de.uni-saarland.syssec.analyzer.analysis
  (:use :cl)
  (:nicknames :analysis)
  (:export state-trace 
	   add-next-state-*
	   make-php-session-history-state
	   make-file-history-state
	   php-sessions
	   current-state
	   diff-history
	   file-diff-entry
	   file-diff-entry-diffs
	   php-session-diff-entry-diffs
	   php-session-diff-entry
	   session-id))


(defpackage de.uni-saarland.syssec.analyzer.php-session 
  (:use :cl)
  (:nicknames :php-session)
  (:export session-id
	   diff
	   parse-php-session
	   extract-session-id))


(defpackage de.uni-saarland.syssec.analyzer.xdebug
  (:use :cl)
  (:nicknames :xdebug)
  (:export make-xdebug-trace
	   get-changed-files-paths
	   get-sql-queries))
