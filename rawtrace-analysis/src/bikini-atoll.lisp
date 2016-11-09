(setf sb-ext:*posix-argv* 
      '("/useless/exec/path"
        "-v" "/home/simkoc/Downloads/Opencart_13_TS_admin_logs_in_and_renames_catgegory-201611072121-vilanoo.db"
        "-m" "/home/simkoc/Downloads/Opencart_13_TS_admin_logs_in_and_renames_catgegory-201611072121-mosgi.db"
        "-d" "/home/simkoc/Downloads/Opencart_13_TS_admin_logs_in_and_renames_catgegory-201611072121-analyzed.db"
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


(clsql:with-database (db-source-connection (list "/home/simkoc/.vilanoo/verification-201611092054-mosgi.db") :database-type :sqlite3)
  (xdebug:get-sql-queries (xdebug:make-xdebug-trace (database:get-xdebug-entry 1 db-source-connection))))
