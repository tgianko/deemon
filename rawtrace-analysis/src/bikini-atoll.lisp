(setf *posix-argv* 
      '("/useless/exec/path"
        "-v" "/home/simkoc/hiwi/csrf/results/Abantecart_TS_11_user_logs_in_and_changes_email-201611072217-vilanoo.db"
        "-m" "/home/simkoc/hiwi/csrf/results/Abantecart_TS_11_user_logs_in_and_changes_email-201611072217-mosgi.db" 
        "-d" "/home/simkoc/hiwi/csrf/results/Abantecart_TS_11_user_logs_in_and_changes_email-201611072217-analyzed.db"
        "-S" "/home/simkoc/hiwi/csrf/vilanoo/data/DBSchema.sql"))


(analyzer:main)
	
#|
(defparameter *test* nil)
      
(clsql:with-database (db (list "/home/simkoc/.vilanoo/blobTestDb.db") :database-type :sqlite3)
  (setf *test* (xdebug:make-xdebug-trace (database:get-xdebug-entry 2 db)))
  nil)
|#

(in-package :database)

(clsql:with-database (db-source-connection 
(database:get-xdebug-entry (car rem-ids) db-source-connection)
