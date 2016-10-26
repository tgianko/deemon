(setf *posix-argv* 
      '("/useless/exec/path"
	"--sink-database" "/home/simkoc/.vilanoo/vilanoo-analysis.db"
	"--source-database" "/home/simkoc/.vilanoo/vilanoo.db"
	"--sink-schema" "/home/simkoc/hiwi/csrf/vilanoo/data/DBSchema.sql"))

(analyzer:main)
	
#|
(defparameter *test* nil)
      
(clsql:with-database (db (list "/home/simkoc/.vilanoo/blobTestDb.db") :database-type :sqlite3)
  (setf *test* (xdebug:make-xdebug-trace (database:get-xdebug-entry 2 db)))
  nil)
|#
