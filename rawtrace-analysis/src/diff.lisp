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
This file contains the generic code for generating a history
of certain aspects one wants to monitor. It should be considered
the general interface.
|#
(in-package :de.uni-saarland.syssec.analyzer.analysis)


(defclass history-state ()
  ((entry-nr
    :initform nil
    :reader entry-nr)))


(defmethod diff-history-state ((old history-state) (new history-state))
  (error "now implementation for base history state"))


(defclass state-trace ()
  ((state-history
    :initform nil
    :accessor state-history)
   (diff-history
    :initform nil
    :accessor diff-history)
   (current-state 
    :initform nil
    :accessor current-state)))


(defun make-empty-state-history ()
  (make-instance 'state-trace))


(defmethod print-object ((st state-trace) stream)
  (with-slots (state-history diff-history current-state)
      st
    (FORMAT stream "state history size:~a~%diff-history-size:~a~%current-state:~%~a"
	    (length state-history) (length diff-history) current-state)))
				 

(defmethod add-next-state-* ((trace state-trace) (new-state history-state))
  (with-slots (state-history diff-history current-state diff-function)
      trace 
    (if (not current-state)
	(progn
	  (setf (slot-value new-state 'entry-nr) 0))
	(progn 
	  (setf (diff-history trace) (append (list (diff-history-state new-state current-state)) (diff-history trace)))
	  (push current-state state-history)
	  (setf (slot-value new-state 'entry-nr) (+ (slot-value (car state-history) 'entry-nr) 1))))
    (setf (slot-value trace 'current-state) new-state)))
  
    

