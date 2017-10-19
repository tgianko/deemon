#|
Author:Simon Koch <s9sikoch@stud.uni-saarland.de>
This file contains code to diff a parsed php-session
and returns a diffed session where each changed element
is a labeled cons

Labels:
:CHANGED -> the content of this element changed
:TYPE-CHAGED -> this element used to be a different element type
:REMOVED -> this element was present in the old session but is not anymore
:ADDED   -> this element was not present in the old session but is now
|#

(in-package :de.uni-saarland.syssec.analyzer.php-session)


(define-condition php-session-diif-condition (simple-error)
  ())


(defmethod diff ((old php-session-array-element) (new php-session-array-element))
  (labels ((diff-array-elements (old-elements new-elements diff-table)
	     (cond 
	       ((not old-elements)
		(mapcar #'(lambda(new-key)
			    (setf (gethash new-key diff-table)
				  (cons (get-element new new-key) :ADDED)))
			new-elements)
		diff-table)
	       ((not new-elements)
		(mapcar #'(lambda(old-key)
			    (setf (gethash old-key diff-table)
				  (cons (get-element old old-key) :REMOVED)))
			old-elements)
		diff-table)
	       ((string> (car old-elements) (car new-elements))
		(setf (gethash (car new-elements) diff-table)
		      (cons (get-element new (car new-elements))
			    :ADDED))
		(diff-array-elements old-elements (cdr new-elements) diff-table))
	       ((string< (car old-elements) (car new-elements))
		(setf (gethash (car old-elements) diff-table)
		      (cons (get-element old (car old-elements))
			    :REMOVED))
		(diff-array-elements (cdr old-elements) new-elements diff-table))
	       (T
		(let ((change-bindings (diff (get-element old (car old-elements))
					    (get-element new (car new-elements)))))
		  (when change-bindings
		    (setf (gethash (car new-elements) diff-table)
			  change-bindings))
		  (diff-array-elements (cdr old-elements) (cdr new-elements) diff-table))))))
    (let ((diff-table (diff-array-elements (sort (get-keys old) #'string<=) (sort (get-keys new) #'string<=) (make-hash-table :test 'equalp))))
      (if (= (hash-table-count diff-table) 0)
	  nil
	  (cons (make-instance 'php-session-array-element :elements diff-table) :CHANGED)))))
		    
	     
(defmethod diff ((old php-session-string-element) (new php-session-string-element))
  (if (string= (content old) (content new))
      nil
      (cons new :CHANGED)))


(defmethod diff ((old php-session-integer-element) (new php-session-integer-element))
  (if (= (int old) (int new))
      nil
      (cons new :CHANGED)))


(defmethod diff ((old php-session-bool-element) (new php-session-bool-element))
  (if (eq (bool old) (bool new))
      nil
      (cons new :CHANGED)))

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


(defmethod diff ((old php-session-content) (new php-session-content))
  (when (eq (content-type old) (content-type new))
    (error 'php-session-diif-condition
	   :format-control "diff between ~a and ~a not yet implemented"
	   :format-arguments (list (content-type old) (content-type new))))
  (cons new :TYPE-CHANGE))


(defmethod diff ((old php-session-element) (new php-session-element))
  (let ((diff-bindings (diff (content old) (content new))))
    (if diff-bindings
	(cons diff-bindings :CHANGED)
	nil)))

(defmethod diff ((old php-session-nil-element) (new php-session-nil-element))
  nil)


(defmethod diff ((old php-session) (new php-session))
  (labels ((diff-elements (old-elements new-elements)
	     (cond 
	       ((not old-elements)
		(mapcar #'(lambda (new)
			    (cons new :ADDED))
			new-elements))
	       ((not new-elements)
		(mapcar #'(lambda (old)
			    (cons old :REMOVED))
			old-elements))
	       ((string> (name (car old-elements))
			 (name (car new-elements)))
		(cons (cons (car new-elements) :ADDED)
		      (diff-elements old-elements (cdr new-elements))))		
	       ((string< (name (car old-elements))
			 (name (car new-elements)))
		(cons (cons (car old-elements) :REMOVED)
		      (diff-elements (cdr old-elements) new-elements)))		
	       (T
		(multiple-value-bind (diff-bindings)
		    (diff (car old-elements)
			  (car new-elements))
		  (if diff-bindings
		      (cons diff-bindings
			    (diff-elements (cdr old-elements)
					   (cdr new-elements)))
		      (diff-elements (cdr old-elements)
				     (cdr new-elements))))))))		      		    
    (when (not (string= (session-id old) (session-id new)))
      (error 'php-session-diff-condition 
	     :format-control "connot compare two different sessions"))
    (let ((diff (diff-elements (elements old)
			       (elements new))))
      (if diff
	  (make-instance 'php-session :elements diff :session-id (session-id new))
	  nil))))
