(in-package :de.uni-saarland.syssec.mosgi.database)

(defparameter +inbetween-buffer+ "/tmp/clsqlbuffer")
(defparameter +inbetween-buffer-query+ "/tmp/clsqlbuffer64-query")

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
    

(defun create-file-query (xdebug-file-path request-db-id)
  ;(with-open-file (stream +inbetween-buffer+ :direction :output :if-does-not-exist :create :if-exists :supersede)
  ;  (FORMAT stream "~a" xdebug-string))
  (with-open-file (stream +inbetween-buffer-query+ :direction :output :if-does-not-exist :create :if-exists :supersede)
    (FORMAT stream "INSERT INTO XDEBUG_DUMPS (HTTP_REQUEST_ID,DUMP_CONTENT) VALUES (~a,\"" request-db-id))
  (FORMAT T "ret:~a~%"
          (trivial-shell:shell-command (FORMAT nil "`/usr/bin/which gzip` ~a --stdout | `/usr/bin/which base64` >> ~a" 
                                               xdebug-file-path
                                               +inbetween-buffer-query+)))
  (with-open-file (stream +inbetween-buffer-query+ :direction :output :if-exists :append)
    (FORMAT stream "\");")))


(defun enter-xdebug-file-into-db (xdebug-file-path request-db-id database-connection com-func)
  (declare (ignore com-func))
  (create-file-query xdebug-file-path request-db-id)
  (funcall com-func (FORMAT nil "Executed cat | sqlite3: ~a"
                            (trivial-shell:shell-command (FORMAT nil "`which cat` ~a | `which sqlite3` ~a"
                                                                 +inbetween-buffer-query+
                                                                 (clsql:database-name database-connection))))))


