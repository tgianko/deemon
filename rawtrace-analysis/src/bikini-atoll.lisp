(setf sb-ext:*posix-argv* 
      '("/useless/exec/path"
        "-v" "/home/simkoc/oxid-user1-buy-201611161817-vilanoo.db"
        "-m" "/home/simkoc/oxid-user1-buy-201611161817-mosgi.db"
        "-d" "/home/simkoc/oxid-user1-buy-201611161817-analyzed.db"
        "-S" "/home/simkoc/hiwi/csrf/vilanoo/data/DBSchema.sql"))




(analyzer:main)
	
#|
(defparameter *test* nil)
      
(clsql:with-database (db (list "/home/simkoc/.vilanoo/blobTestDb.db") :database-type :sqlite3)
  (setf *test* (xdebug:make-xdebug-trace (database:get-xdebug-entry 2 db)))
  nil)
|#

(in-package :database)

(FORMAT T "~a~%"

(defun dump (n)
  (with-open-file (str (FORMAT nil "/home/simkoc/Downloads/xdebug-~a" n) :direction :output :if-does-not-exist :create)
    (clsql:with-database (db-source-connection (list "/home/simkoc/.vilanoo/verification-201611092054-mosgi.db") :database-type :sqlite3)
      (FORMAT str "~a~%" (database:get-xdebug-entry n db-source-connection)))))

(dump 1)

(dump 2)

(defparameter *test-session*
  (cadr (nth 3 
   (clsql:with-database (db-source-connection (list "/home/simkoc/oxid-user1-buy-201611161817-mosgi.db") :database-type :sqlite3)
     (database:get-all-session-entries 1 db-source-connection)))))
  
(php-session:parse-php-session (make-string-input-stream *test-session*) "test") 

(with-open-file (stream "~/session_stuff" :element-type '(unsigned-byte 8) :direction :output :if-does-not-exist :create)
  (FORMAT stream "~a" *test-session*))

(FORMAT T "~a~%~%" (subseq *weird-sub* 0 2708))

(defparameter *weird-sub* (subseq *test-session* 233))



(clsql:with-database (db-source-connection (list "/home/simkoc/.vilanoo/verification-201611092054-mosgi.db") :database-type :sqlite3)
  (xdebug:get-sql-queries (xdebug:make-xdebug-trace (database:get-xdebug-entry 1 db-source-connection))))


