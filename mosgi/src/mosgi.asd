(asdf:defsystem :mosgi
    :description "on-line interface to analyse and utilize xdebug output (and php-sessions) for vilanoo"
    :version "1.0"
    :author "Simon Koch <s9sikoch@stud.uni-saarland.de>"
    :depends-on (:libssh2
		 :cl-ppcre
		 :unix-opts
		 :clsql
		 :sb-concurrency
                 :gzip-stream
                 :cl-base64)
    :components ((:file "packages")
		 (:file "communication" :depends-on ("packages"))
		 (:file "ssh-interface" :depends-on ("packages"))
		 (:file "database" :depends-on ("packages"))
		 (:file "main" :depends-on ("packages"
					    "communication"
					    "database"
					    "ssh-interface"))))
