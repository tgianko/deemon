(in-package :de.uni-saarland.syssec.mosgi.database)


(clsql:file-enable-sql-reader-syntax)


(defun enter-sessions-raw-into-db (session-file-string-id-pairs request-db-id database-connection com-func)
  (declare (ignore com-func))
  (do ((string-list session-file-string-id-pairs (cdr string-list))
       (counter 0 (+ 1 counter)))
      ((not string-list) nil)
    (clsql:insert-records :INTO [SESSIONS]
			  :ATTRIBUTES '([HTTP-REQUEST-ID] [SESSION-NAME] [SESSION-STRING])
			  :VALUES (list request-db-id (caar string-list) (cl-base64:string-to-base64-string (cdar string-list)))
			  :database database-connection)))
    


(clsql:def-view-class xdebug-dumps ()
  ((http-request-id
    :type (integer)
    :accessor http-request-id
    :initarg :http-request-id)
   (dump-content
    :type (array)
    :accessor dump-content
    :initarg :dump-content)))



(defun enter-xdebug-file-raw-into-db (xdebug-blob request-db-id database-connection com-func)
  (declare (ignore com-func))
  (let ((instance (make-instance 'xdebug-dumps
                                 :dump-content (gzip-stream:gzip-sequence xdebug-blob) ;I hate this scheme but compression is needed else heap exhaustion
                                 :http-request-id request-db-id)))
    (clsql:update-records-from-instance instance :database database-connection)))
