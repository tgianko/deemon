(defpackage :de.uni-saarland.syssec.mosgi
  (:use :cl)
  (:nicknames mosgi)
  (:export main))


(defpackage :de.uni-saarland.syssec.mosgi.diff
  (:use :cl)
  (:nicknames diff)
  (:export add-next-state-*
	   make-file-history-state
	   make-php-session-history-state
	   state-trace
	   php-session-state
	   make-empty-state-history))


(defpackage :de.uni-saarland.syssec.mosgi.ssh-interface
  (:use :cl)
  (:nicknames ssh)
  (:export scp
	   folder-content-guest
	   run-remote-shell-command))


(defpackage :de.uni-saarland.syssec.mosgi.php-session
  (:use :cl)
  (:nicknames :php-session)
  (:export diff-sessions
	   new-session
	   deleted-session
	   parse-php-session
	   session-id
	   elements
	   extract-session-id
	   diff))
	   

(defpackage :de.uni-saarland.syssec.mosgi.xdebug
  (:use :cl)
  (:nicknames :xdebug)
  (:export get-changed-files-paths
	   make-xdebug-trace
	   get-xdebug-trace-file))


(defpackage :de.uni-saarland.syssec.mosgi.communication
  (:use :cl)
  (:nicknames :com)
  (:export with-connected-communication-handler
	   receive-character
	   send-character))
	   
