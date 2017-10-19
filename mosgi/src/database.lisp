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

(in-package :de.uni-saarland.syssec.mosgi.database)

(defparameter +inbetween-buffer+ "/tmp/clsqlbuffer")
(defparameter +inbetween-buffer-query+ "/tmp/clsqlbuffer64-query")

(clsql:file-enable-sql-reader-syntax)


(defun enter-sessions-raw-into-db (session-file-string-id-pairs request-db-id database-connection com-func)
  (declare (ignore com-func))
  (do ((string-list session-file-string-id-pairs (cdr string-list))
       (counter 0 (+ 1 counter)))
      ((not string-list) nil)
    (clsql:insert-records :INTO [SESSIONS]
			  :ATTRIBUTES '([HTTP-REQUEST-ID] [SESSION-NAME] [SESSION-STRING])
			  :VALUES (list request-db-id (caar string-list) (cl-base64:string-to-base64-string (cdar string-list)))
			  :database database-connection)))
    

(defun create-file-query (xdebug-file-path request-db-id)
  (with-open-file (stream +inbetween-buffer-query+ :direction :output :if-does-not-exist :create :if-exists :supersede)
    (FORMAT stream "INSERT INTO XDEBUG_DUMPS (HTTP_REQUEST_ID,DUMP_CONTENT) VALUES (~a,\"" request-db-id))
  (FORMAT T "ret:~a~%"
          (trivial-shell:shell-command (FORMAT nil "`/usr/bin/which gzip` ~a --stdout | `/usr/bin/which base64` >> ~a" 
                                               xdebug-file-path
                                               +inbetween-buffer-query+)))
  (with-open-file (stream +inbetween-buffer-query+ :direction :output :if-exists :append)
    (FORMAT stream "\");")))


(defun enter-xdebug-file-into-db (xdebug-file-path request-db-id database-connection com-func)
  (declare (ignore com-func))
  (create-file-query xdebug-file-path request-db-id)
  (funcall com-func (FORMAT nil "Executed cat | sqlite3: ~a"
                            (trivial-shell:shell-command (FORMAT nil "`which cat` ~a | `which sqlite3` ~a"
                                                                 +inbetween-buffer-query+
                                                                 (clsql:database-name database-connection))))))
