(setf *posix-argv* '("./call/path" 
		     "--php-session-folder" "/opt/bitnami/php/tmp/"
		     "--xdebug-trace-file" "/tmp/xdebug.xt"
		     "--port" "9292"
		     "--interface" "127.0.0.1"
		     "--target-system" "192.168.56.105"
		     "--target-root" "root"
		     "--sql-db-path" "/home/simkoc/.vilanoo/vilanoo.db"
		     "--host-pwd" "bitnami"))


(setf *posix-argv* '("--php-session-folder" "/opt/bitnami/php/tmp/" 
		     "--xdebug-trace-file" "/tmp/xdebug.xt" 
		     "--port" "9292" 
		     "--interface" "127.0.0.1" 
		     "--target-system" "192.168.56.102" 
		     "--target-root" "root" 
		     "--host-pwd" "bitnami" 
		     "--sql-db-path" "/home/simkoc/.vilanoo/vilanoo.db"))

(mosgi:main)



(clsql:with-database (db (list "/home/simkoc/.vilanoo/blobTestDb.db") :database-type :sqlite3)
  (database:enter-xdebug-file-raw-into-db 
   (ssh:get-file-as-blob "/tmp/test.t"
                         "root"
                         "192.168.56.101"
                         "bitnami")
   1
   db
   #'(lambda (string)
       (FORMAT T "~a~%" string))))
 

