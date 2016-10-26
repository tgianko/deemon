(setf *posix-argv* 
      '("/useless/exec/path"
        "-v" "/home/simkoc/.vilanoo/oxidTS01-change-password-201610261737-vilanoo.db"
        "-m" "/home/simkoc/.vilanoo/oxidTS01-change-password-201610261737-mosgi.db" 
        "-d" "/home/simkoc/.vilanoo/oxidTS01-change-password-201610261737-analyzed.db"
        "-S" "/home/simkoc/hiwi/csrf/vilanoo/data/DBSchema.sql"))


(analyzer:main)
	
#|
(defparameter *test* nil)
      
(clsql:with-database (db (list "/home/simkoc/.vilanoo/blobTestDb.db") :database-type :sqlite3)
  (setf *test* (xdebug:make-xdebug-trace (database:get-xdebug-entry 2 db)))
  nil)
|#
