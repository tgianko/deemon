(asdf:defsystem :mosgi
    :description "tool to interface with vilanoo to save/retrieve xdebug/session data in accordance with selenesecommands and store them in a sqlite database"
    :version "2.0"
    :author "Simon Koch <s9sikoch@stud.uni-saarland.de>"
    :depends-on (:trivial-ssh
		 :cl-ppcre
		 :unix-opts
		 :clsql
		 :sb-concurrency
                 :gzip-stream
                 :cl-base64
                 :trivial-shell)
    :components ((:file "packages")
		 (:file "communication" :depends-on ("packages"))
		 (:file "ssh-interface" :depends-on ("packages"))
		 (:file "database" :depends-on ("packages"))
		 (:file "main" :depends-on ("packages"
					    "communication"
					    "database"
					    "ssh-interface"))))
