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

#|
This file provides code to generate and diff file-history-states for
clustering of page requests
|#
(in-package :de.uni-saarland.syssec.analyzer.analysis)
	   

(defstruct file-diff-entry diffs)


(defclass file-history-state (history-state)
  ((file-index 
    :initarg :file-index
    :reader file-index)))


(defmethod print-object ((object file-history-state) stream)
  (FORMAT stream "[~{~a~^~%~}]" (file-index object)))
   

(defun make-file-history-state (relevant-file-paths)
  (make-instance 'file-history-state
		 :file-index (remove-duplicates
			      (sort 
			       relevant-file-paths
			       #'string-lessp)
			      :test #'string=)))
  

(defmethod diff-history-state ((new file-history-state) (current file-history-state))
  (declare (ignore current))
  (make-file-diff-entry :diffs (file-index new)))
