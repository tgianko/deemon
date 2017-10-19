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
