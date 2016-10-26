(asdf:defsystem :analyzer
    :description "analyzes the trace data previously retrieved by mosgi"
    :version "1.0"
    :author "Simon Koch <s9sikoch@stud.uni-saarland.de>"
    :depends-on (:cl-ppcre
		 :unix-opts
		 :clsql
		 :cl-fad
                 :gzip-stream
                 :flexi-streams
                 :cl-base64)
    :components ((:file "packages")
		 (:file "php-session" :depends-on ("packages"))
		 (:file "php-session-diff" :depends-on ("packages"
							"php-session"))
		 (:file "xdebug-parser" :depends-on ("packages"))
		 (:file "diff" :depends-on ("packages"
					    "php-session"
					    "php-session-diff"
					    "xdebug-parser"))
		 (:file "diff-php" :depends-on ("packages"
						"diff"))
		 (:file "diff-file" :depends-on ("packages"
						 "diff"))		 		       
		 (:file "database" :depends-on ("packages"
						"diff-php"
						"diff-file"
						"diff"))
		 (:file "main" :depends-on ("packages"
					    "database"
					    "diff-php"
					    "diff-file"
					    "xdebug-parser"))))
					    
