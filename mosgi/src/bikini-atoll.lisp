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


(defun stream->string (tmp-stream)
  (do ((line (read-line tmp-stream nil nil)
             (read-line tmp-stream nil nil))
       (lines nil))
      ((not line) (progn 
                    (FORMAT T "COLLECTED~%")
                    (FORMAT nil "~{~a~^~%~}" (reverse lines))))
    (push line lines)))


(ssh:convert-to-utf8-encoding "/home/simkoc/hiwi/csrf/debugFiles/xdebug_1.xt")


(defparameter *test* nil)

(with-open-file (stream "/home/simkoc/hiwi/csrf/debugFiles/xdebug_1.xt")
  (room)
  (setf *test* (stream->string stream))
  (sb-ext:gc :full t)
  (room))
        

(with-open-file (stream "/home/simkoc/hiwi/csrf/debugFiles/xdebug_1.xt")
  (clsql:with-database (db (list "/home/simkoc/.vilanoo/oxidTS01-change-email-201610201656-mosgi.db") :database-type :sqlite3)
    (database:enter-xdebug-file-raw-into-db *test* 42 db #'(lambda(string)
                                                             (FORMAT T "~a~%" string)))))
