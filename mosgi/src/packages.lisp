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

(defpackage :de.uni-saarland.syssec.mosgi
  (:use :cl)
  (:nicknames mosgi)
  (:export main
	   print-threaded))


(defpackage :de.uni-saarland.syssec.mosgi.ssh-interface
  (:use :cl)
  (:nicknames ssh-interface)
  (:export scp
           with-active-ssh-connection
           *global-ssh-connection*
	   folder-content-guest
	   run-remote-shell-command
	   convert-to-utf8-encoding
	   backup-all-files-from
           get-file-as-simple-string
           get-file-as-file
	   backup-file
	   delete-folder	   
	   register-machine
	   probe-machine
	   get-all-contained-files-as-strings
	   get-file-as-string
           get-file-as-blob
           get-all-contained-files-as-base64-blob
           move-file))


(defpackage :de.uni-saarland.syssec.mosgi.communication
  (:use :cl)
  (:nicknames :com)
  (:export with-connected-communication-handler
	   receive-byte
	   send-byte
	   receive-32b-unsigned-integer
	   receive-64b-unsigned-integer))


(defpackage :de.uni-saarland.syssec.mosgi.database
  (:use :cl)
  (:nicknames :database)
  (:export enter-sessions-raw-into-db
	   enter-xdebug-file-into-db))
	   
