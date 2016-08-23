(defpackage :de.uni-saarland.syssec.mosgi
  (:use :cl)
  (:nicknames mosgi)
  (:export main
	   print-threaded))


(defpackage :de.uni-saarland.syssec.mosgi.ssh-interface
  (:use :cl)
  (:nicknames ssh)
  (:export scp
	   folder-content-guest
	   run-remote-shell-command
	   convert-to-utf8-encoding
	   backup-all-files-from
	   backup-file
	   delete-folder	   
	   register-machine
	   probe-machine
	   get-all-contained-files-as-strings
	   get-file-as-string))


(defpackage :de.uni-saarland.syssec.mosgi.communication
  (:use :cl)
  (:nicknames :com)
  (:export with-connected-communication-handler
	   receive-byte
	   send-byte
	   receive-32b-unsigned-integer
	   receive-64b-unsigned-integer))


(defpackage :de.uni-saarland.syssec.mosgi.database
  (:use :cl)
  (:nicknames :database)
  (:export enter-sessions-raw-into-db
	   enter-xdebug-file-raw-into-db))
	   
