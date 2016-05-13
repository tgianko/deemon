(asdf:defsystem :mosgi
    :description "on-line interface to analyse and utilize xdebug output (and php-sessions) for vilanoo"
    :version "1.0"
    :author "Simon Koch <s9sikoch@stud.uni-saarland.de>"
    :depends-on (:libssh2
		 :cl-ppcre
		 :unix-opts)
    :components ((:file "packages")
		 (:file "communication" :depends-on ("packages"))
		 (:file "php-session" :depends-on ("packages"))
		 (:file "xdebug-parser" :depends-on ("packages"))
		 (:file "diff" :depends-on ("packages"))
		 (:file "diff-file" :depends-on ("packages"
						 "diff"))
		 (:file "diff-php" :depends-on ("packages"
						"diff"
						"php-session"))
		 (:file "ssh-interface" :depends-on ("packages"))
		 (:file "main" :depends-on ("packages"
					    "ssh-interface"
					    "php-session"
					    "communication"))))
		    
