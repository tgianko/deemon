#|
Author:Simon Koch <s9sikoch@stud.uni-saarland.de>
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
