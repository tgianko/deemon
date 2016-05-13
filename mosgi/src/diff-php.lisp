(in-package :de.uni-saarland.syssec.mosgi.diff)


(defstruct php-session-diff-entry diffs)
	   

(defclass php-session-state (history-state)
  ((php-sessions
    :initarg :php-sessions
    :reader php-sessions)))


(defmethod diff-history-state ((old php-session-state) (new php-session-state))
  (labels ((php-sessions-diff (old-sessions new-sessions) ;assumes string<= ordered indexes - true by definition of file-history-state
	     (cond 
	       ((not old-sessions)
		(mapcar #'(lambda (session) (cons (php-session:session-id session) (php-session:new-session session))) new-sessions))
	       ((not new-sessions)
		(mapcar #'(lambda (session) (cons (php-session:session-id session) (php-session:deleted-session session))) old-sessions))
	       ((string= (php-session:session-id (car old-sessions))
			 (php-session:session-id (car new-sessions)))
		(let ((diff (php-session:diff-sessions (car old-sessions) (car new-sessions))))
		  (if diff 
		      (cons (cons (php-session:session-id (car old-sessions)) diff)
			    (php-sessions-diff (cdr old-sessions) (cdr new-sessions)))
		      (php-sessions-diff (cdr old-sessions) (cdr new-sessions)))))
	       ((string< (php-session:session-id (car old-sessions))
			 (php-session:session-id (car new-sessions)))
		(cons (cons (php-session:session-id (car old-sessions)) (php-session:deleted-session (car old-sessions)))
		      (php-sessions-diff (cdr old-sessions) new-sessions)))
	       ((string> (php-session:session-id (car old-sessions))
			 (php-session:session-id (car new-sessions)))
		(cons (cons (php-session:session-id (car new-sessions)) (php-session:new-session (car new-sessions)))
		      (php-sessions-diff old-sessions (cdr new-sessions)))))))
    (make-php-session-diff-entry :diffs (php-sessions-diff (php-sessions old) (php-sessions new)))))


(defun make-php-session-history-state (php-session-folder user host pwd)
  (make-instance 'php-session-state
		 :php-sessions (sort (mapcar #'(lambda(php-session-file)
						 (cl-fad:with-open-temporary-file (stream)
						   (ssh:scp php-session-file (pathname stream) user host pwd)
						   (finish-output stream)
						   (php-session:make-php-session stream (php-session:extract-session-id php-session-file))))
					     (ssh:folder-content-guest php-session-folder user host pwd))
				     #'string<=
				     :key #'php-session:session-id)))
