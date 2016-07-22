(defpackage :de.uni-saarland.syssec.clustifier
  (:use :cl)
  (:nicknames :clustifier)
  (:export main))


(defpackage :de.uni-saarland.syssec.clustifier.database
  (:use :cl)
  (:nicknames :database)
  (:export http-request-id
	   sql-hash
	   file-hash
	   session-hash
	   get-all-requests))


(defpackage :de.uni-saarland.syssec.clustifier.result-printer
  (:use :cl)
  (:nicknames :print)
  (:export dump-results-into-folder))
