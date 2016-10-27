(asdf:defsystem :mosgi
    :description "on-line tool to interface with vilanoo to save/retrieve xdebug/session data in accordance with selenesecommands"
    :version "2.0"
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
