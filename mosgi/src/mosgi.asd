(asdf:define-system :mosgi
    :description "on-line interface to analyse and utilize xdebug output (and php-sessions) for vilanoo"
    :version "1.0"
    :author "Simon Koch <s9sikoch@stud.uni-saarland.de>"
    :components ((:file "packages")
		 (:file "ssh-interface" :depends-on ("packages"))
		 (:file "php-session" :depends-on ("packages"))))
		    
