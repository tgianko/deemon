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
    


(clsql:def-view-class xdebug-dumps ()
  ((http-request-id
    :type (integer)
    :accessor http-request-id
    :initarg :http-request-id)
   (dump-content
    :type (array)
    :accessor dump-content
    :initarg :dump-content)))


(defparameter *subsequence-size* 1000000)


(defun enter-xdebug-file-raw-into-db (xdebug-blob request-db-id database-connection com-func)
  (declare (ignore com-func))
  (let ((instance (make-instance 'xdebug-dumps
                                 :dump-content xdebug-blob
                                 :http-request-id request-db-id)))
    (clsql:update-records-from-instance instance :database database-connection)))


#|
  (clsql:insert-records :INTO [XDEBUG-DUMPS]
			:ATTRIBUTES '([HTTP-REQUEST-ID] [DUMP-CONTENT])
			:VALUES (list request-db-id xdebug-blob)
			:database database-connection))
|#


(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))


#|
|#
