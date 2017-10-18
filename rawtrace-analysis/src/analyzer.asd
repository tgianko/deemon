; This file is part of Deemon.

; Deemon is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; Deemon is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with Deemon.  If not, see <http://www.gnu.org/licenses/>.

(asdf:defsystem :analyzer
    :description "analyzes the trace data previously retrieved by mosgi and extracts queries"
    :version "1.0"
    :author "Simon Koch <s9sikoch@stud.uni-saarland.de>"
    :depends-on (:cl-ppcre
		 :unix-opts
		 :clsql
		 :cl-fad
                 :gzip-stream
                 :flexi-streams
                 :cl-base64
                 :trivial-shell)
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
					    
