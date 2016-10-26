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


#|
(defun get-blob (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (let ((sequence (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence sequence stream)
      sequence)))
    

(defparameter *test* nil)

(progn
  (setf *test* (gzip-stream:gzip-sequence (get-blob "/home/simkoc/hiwi/csrf/debugFiles/xdebug_1.xt")))
  nil)


(defparameter *test-2* nil)

(progn 
  (setf *test-2* (get-blob "/home/simkoc/hiwi/csrf/debugFiles/xdebug_1.xt"))
  nil)
        
(clsql:with-database (db (list "/home/simkoc/.vilanoo/blobTestDb.db") :database-type :sqlite3)
  (database:enter-xdebug-file-raw-into-db *test*
                                          2
                                          db
                                          #'(lambda (string)
                                              (FORMAT T "~a~%" string))))

(sb-thread:destroy-thread (cadr (sb-thread:list-all-threads)))
|#
