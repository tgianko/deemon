(defpackage :de.uni-saarland.syssec.mosgi)


(defpackage :de.uni-saarland.syssec.mosgi.ssh-interface
  (:use :cl)
  (:nicknames file-transfer)
  (:export scp))


(defpackage :de.uni-saarland.syssec.mosgi.php-session
  (:use :cl)
  (:nicknames :php-session)
  (:export diff-sessions
	   make-php-session
	   elements))
	   

(defpackage :de.uni-saarland.syssec.mosgi.file-diff 
  (:use :cl)
  (:nicknames :file-diff)
  (:export change-amount-byte))


(defpackage :de.uni-saarland.syssec.mosgi.xdebug
  (:use :cl)
  (:nicknames :xdebug-parser)
  (:export get-file-changes))
