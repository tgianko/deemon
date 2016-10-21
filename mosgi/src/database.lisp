(in-package :de.uni-saarland.syssec.mosgi.database)


(clsql:file-enable-sql-reader-syntax)


(defun enter-sessions-raw-into-db (session-file-string-id-pairs request-db-id database-connection com-func)
  (declare (ignore com-func))
  (do ((string-list session-file-string-id-pairs (cdr string-list))
       (counter 0 (+ 1 counter)))
      ((not string-list) nil)
    (clsql:insert-records :INTO [SESSIONS]
			  :ATTRIBUTES '([HTTP-REQUEST-ID] [SESSION-NAME] [SESSION-STRING])
			  :VALUES (list request-db-id (caar string-list) (cdar string-list))
			  :database database-connection)))
    


(defun enter-xdebug-file-raw-into-db (xdebug-file-string request-db-id database-connection com-func)
  (declare (ignore com-func))
  (sb-ext:gc :full t)
  (FORMAT T "about to insert~%")
  (clsql:insert-records :INTO [XDEBUG-DUMPS]
			:ATTRIBUTES '([HTTP-REQUEST-ID] [DUMP-CONTENT])
			:VALUES (list request-db-id xdebug-file-string)
			:database database-connection))
