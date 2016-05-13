(in-package :de.uni-saarland.syssec.mosgi.php-session)

#|
The session elements can be interpreted as a tree, thus, in the following
the diff will be defined by traversing the tree and dropping every equal
element. Remaining elements will be kept and a label added that will either
read :DELETED :ADDED :CHANGED

:DELETED -> element was in old session but not in new
:ADDED   -> element was not in old session but is in new
:CONTENT-CHANGED -> element is present in both sessions but is not equal
:TYPE-CHANGED -> element is present in both sessions but type changed

Any change will carry through to the root node (defined by the content of the elements table)
thus if a leaf is changed, so is the whole branch of the tree up to the root. Elements that do
not change will be dropped in the diff even if they are contained in a changed branch.
|#


#| OKEY I AM REALLY LOST - NEED A BREAK 
Basic Idea - recursion through the session a build a diff-tree but as I do not chose
to do a pure list representation of the session content for arrays/sub-arrays I run into
type problems. Either change the creation of sessions and drop hashtables or ensure that
no typeproblems occure by ensuring same type on the same level of the tree even if
there was an addition/deletion
|#

(defun get-keys (hash-table)
  (sort 
   (loop for key being the hash-keys of hash-table collect key)
   #'string<=))


(defmethod new-session ((new php-session))
  (diff-sessions (create-empty-php-session (session-id new)) new))


(defmethod deleted-session ((deleted php-session))
  (diff-sessions deleted (create-empty-php-session (session-id deleted))))


(defun diff-array (old-array new-array)
  (let ((changes-p nil))
    (labels ((diff-array-elements (old-array-keys new-array-keys)
	       (cond
		 ((not old-element-keys)
		  )
		 ((not new-element-keys)
		  )
		 ((string= (car old-element-keys) (car new-element-keys))
		  )
		 ((string< (car old-element-keys) (car new-element-keys))
		  )
		 ((string> (car old-element-keys) (car new-element-keys))
		  ))))
      (values (diff-array-elements (get-keys old-array)
				   (get-keys new-array))
	      changes-p))))


(defun diff-element (old-element new-element)
  (cond 
    ((and (not old-element)
	  new-element)
     (values (list (car new-element) (cdr new-element) :ADDED) T))
    ((and old-element
	  (not new-element))
     (values (list (car old-element) (cdr old-element) :DELETED) T))
    ((equalp (car old-element) (car new-element)) ;check if it is still the same type
     (case (car old-element)
       (:ARRAY 
	(multiple-value-bind (array-diff changed)
	    (diff-array old-element new-element)
	  (if changed 
	      (values (list (car old-element) array-diff :CONTENT-CHANGED) T)
	      (values old-element nil))))
       (:STRING
	(if (string= (cdr old-element) (cdr new-element))
	    (values old-element nil)
	    (values (list (car new-element) (cdr new-element) :CONTENT-CHANGED) T)))
       (:EMPTY ;empty cannot change as it is supposed to be empty - aint it?
	(values old-element nil))))
    (T
     (values (list (car new-element) (cdr new-element) :TYPE-CHANGE) T))))


(defmethod diff-sessions ((old php-session) (new php-session))  
  (let ((old-elements-table (elements old))
	(new-elements-table (elements new)))
    (labels ((diff-elements (old-element-keys new-element-keys)
	       (cond
		 ((not old-element-keys)
		  (mapcar #'(lambda (new-element-key)
			      (diff-element nil (gethash new-element-key new-element-table)))
			  new-element-keys))			  
		 ((not new-element-keys)
		  (mapcar #'(lambda (old-element-key)
			      (diff-element (gethash old-element-key old-elements-table) nil))))
		 ((string= (car old-element-keys) (car new-element-keys))
		  (multiple-value-bind (diff-value changed-p)
		      (diff-element (gethash (car old-element-keys) old-elements-table)
				    (gethash (car new-element-keys) new-elements-table))
		    (if changed-p
			(cons (list (car old-element-key) diff-value :CHANGED)
			      (diff-elements (cdr old-element-keys) (cdr new-element-keys))))))			
		 ((string< (car old-element-keys) (car new-element-keys))
		  )
		 ((string> (car old-element-keys) (car new-element-keys))
		  ))))
      (diff-elements (get-keys old-elements-table)
		     (get-keys new-elements-table)))))


