(in-package :de.uni-saarland.syssec.mosgi.diff)


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


(defmethod add-next-state-* ((trace state-trace) (new-state history-state))
  (with-slots (state-history diff-history current-state diff-function)
      trace 
    (if (not state-history)
	(progn
	  (setf (slot-value new-state 'entry-nr) 0))
	(progn 
	  (push (diff-history-state new-state (car state-history)) diff-history)
	  (push current-state state-history)
	  (setf (slot-value new-state 'entry-nr) (+ (slot-value (car state-history) 'entry-nr) 1))))
    (setf current-state new-state)))
  
    

