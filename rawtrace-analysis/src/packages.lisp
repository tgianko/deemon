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


(defpackage de.uni-saarland.syssec.analyzer
  (:use :cl)
  (:nicknames :analyzer)
  (:export main))


(defpackage de.uni-saarland.syssec.analyzer.database
  (:use :cl)
  (:nicknames :database)
  (:export create-database
	   copy-http-request-entries
	   get-highest-http-request-id-entry
	   get-all-http-request-ids
	   get-all-session-entries
	   get-xdebug-entry
           commit-sql-queries
	   commit-latest-diff
	   commit-full-sessions
	   commit-latest-diff
	   merge-databases
           get-xdebug-entry-as-file-path
           commit-raw-sessions))


(defpackage de.uni-saarland.syssec.analyzer.analysis
  (:use :cl)
  (:nicknames :analysis)
  (:export state-trace 
	   add-next-state-*
	   make-php-session-history-state
	   make-file-history-state
	   php-sessions
	   current-state
	   diff-history
	   file-diff-entry
	   file-diff-entry-diffs
	   php-session-diff-entry-diffs
	   php-session-diff-entry
	   session-id))


(defpackage de.uni-saarland.syssec.analyzer.php-session 
  (:use :cl)
  (:nicknames :php-session)
  (:export session-id
	   diff
	   parse-php-session
	   extract-session-id))


(defpackage de.uni-saarland.syssec.analyzer.xdebug
  (:use :cl)
  (:nicknames :xdebug)
  (:export make-xdebug-trace
	   get-changed-files-paths
	   get-sql-queries
           make-xdebug-trace-from-file))
