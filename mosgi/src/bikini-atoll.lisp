(setf *posix-argv* '("./call/path" 
		     "--php-session-folder" "/opt/bitnami/php/tmp/"
		     "--xdebug-trace-folder" "/tmp/"
		     "--port" "9292"
		     "--interface" "127.0.0.1"
		     "--target-system" "192.168.56.106"
		     "--target-root" "root"
		     "--host-pwd" "bitnami"))


(mosgi:main)
