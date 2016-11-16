(setf *posix-argv* '("./call/path" 
		     "--php-session-folder" "/opt/bitnami/php/tmp/"
		     "--xdebug-trace-file" "/tmp/xdebug.xt"
		     "--port" "9292"
		     "--interface" "127.0.0.1"
		     "--target-system" "192.168.56.101"
		     "--target-root" "root"
		     "--sql-db-path" "/home/simkoc/.vilanoo/vilanoo.db"
		     "--host-pwd" "bitnami"))


(mosgi:main)


(ssh-interface:with-active-ssh-connection ("root" "192.168.56.101" "bitnami")
  (ssh-interface:folder-content-guest "/tmp/" "" "" ""))


(ssh-interface:with-active-ssh-connection ("root" "192.168.56.101" "bitnami")
  (ssh-interface:run-remote-shell-command "rm -f /tmp/*" "" "" "" #'(lambda (stream)
								   (declare (ignore stream)))))
