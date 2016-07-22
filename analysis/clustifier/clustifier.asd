(asdf:defsystem :clustifier
  :description "this thingy is awesome - see -h for more awesome information. Did I mention it is awesome?"
  :author "Simon Koch <s9sikoch@stud.uni-saarland.de"
  :version "1.0"
  :depends-on (:ironclad
	       :unix-opts
	       :clsql
	       :cl-ppcre)
  :components ((:file "packages")
	       (:file "database" :depends-on ("packages"))
	       (:file "result-printer" :depends-on ("packages"
						    "database"))
	       (:file "main" :depends-on ("packages"
					   "database"
					   "result-printer"))))
