(asdf:defsystem :mosgi
    :description "on-line interface to analyse and utilize xdebug output (and php-sessions) for vilanoo"
    :version "1.0"
    :author "Simon Koch <s9sikoch@stud.uni-saarland.de>"
    :depends-on (:libssh2
		 :cl-ppcre
		 :unix-opts
		 :clsql
		 :sb-concurrency)
    :components ((:file "packages")
		 (:file "communication" :depends-on ("packages"))
		 (:file "php-session" :depends-on ("packages"))
		 (:file "php-session-diff" :depends-on ("packages"
							"php-session"))
		 (:file "xdebug-parser" :depends-on ("packages"))
		 (:file "diff" :depends-on ("packages"))
		 (:file "diff-file" :depends-on ("packages"
						 "diff"))
		 (:file "diff-php" :depends-on ("packages"
						"diff"
						"php-session"
						"php-session-diff"))
		 (:file "ssh-interface" :depends-on ("packages"))
		 (:file "database" :depends-on ("packages"
						"diff-file"
						"diff-php"))
		 (:file "main" :depends-on ("packages"
					    "ssh-interface"
					    "php-session"
					    "communication"))))
		    
