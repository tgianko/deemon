#|
Author: Simon Koch <s9sikoch@stud.uni-saarland.de>
This file contains the code to parse xdebug trace files with 
trace_format = 1. 
It also provides the means to extract all fopen calls of the
given trace and returns all parameters passed to those calls.
|#
(in-package :de.uni-saarland.syssec.mosgi.xdebug)


(defun get-xdebug-trace-file (folder-files)
  (let ((rel-files (remove-if-not #'(lambda (file-path)
				      (cl-ppcre:scan ".*/trace\.[0-9]+\.xt" file-path))
				  folder-files)))
    (when (> (length rel-files) 1)
      (warn "more than one possible trace file detected - select lexicographical first"))
    (car rel-files)))
	  


(defclass xdebug-trace ()
  ((trace-content
    :initarg :trace-content
    :reader trace-content)))


(defclass record ()
  ((level
    :initarg :level
    :reader level)
   (function-nr
    :initarg :function-nr
    :reader function-nr)))
   

(defclass entry-record (record)
  ((time-index
    :initarg :time-index
    :reader time-index)
   (memory-usage
    :initarg :memory-usage
    :reader memory-usage)
   (function-name
    :initarg :function-name
    :reader function-name)
   (user-defined-p
    :initarg :user-defined-p
    :reader user-defined-p)
   (name-of-ir-file
    :initarg :name-of-ir-file
    :reader name-of-ir-file)
   (filename 
    :initarg :filename
    :reader filename)
   (line-number
    :initarg :line-number
    :reader line-number)
   (parameters
    :initarg :parameters
    :reader parameters)))


(defclass exit-record (record)
  ((time-index
    :initarg :time-index
    :reader time-index)
   (memory-usage
    :initarg :memory-usage
    :reader memory-usage)))


(defclass return-record (record)
  ((return-value
    :initarg :return-value
    :reader return-value)))


(defun make-entry-records (entries)
  (destructuring-bind (level function-nr always time-index memory-usage function-name user-defined-p name-of-ir-file filename line-number param-count &rest params)
      entries
    (declare (ignore always param-count))
    (make-instance 'entry-record
		   :level level 
		   :function-nr function-nr 
		   :time-index time-index
		   :memory-usage memory-usage
		   :function-name function-name
		   :user-defined-p user-defined-p
		   :name-of-ir-file name-of-ir-file
		   :filename filename
		   :line-number line-number
		   :parameters params)))


(defun make-exit-records (entries)
  (destructuring-bind (level function-nr always time-index memory-usage)
      entries
    (declare (ignore always))
    (make-instance 'exit-record
		   :level level 
		   :function-nr function-nr 
		   :time-index time-index
		   :memory-usage memory-usage)))


(defun make-return-records (entries)
  (destructuring-bind (level function-nr always e1 e2 return-value)
      entries
    (declare (ignore always e1 e2))
    (make-instance 'return-record
		   :level level
		   :function-nr function-nr
		   :return-value return-value)))


(defun parse-xdebug-trace-line (line)
  (destructuring-bind (level function-nr always &rest rest)
      (cl-ppcre:split "\\t" line)
    (cond
      ((and (string= "" level)
	    (string= "" function-nr)
	    (string= "" always))
       nil)
      ((string= "0" always)
       (make-entry-records (append (list level function-nr always) rest)))
      ((string= "1" always)
       (make-exit-records (append (list level function-nr always) rest)))
      ((string= "R" always)
       (make-return-records (append (list level function-nr always) rest)))
      (T
       (error 'simple-error 
	      :format-control "there aint no such thing as ~a as the always record part in ~a"
	      :format-arguments (list always line))))))
       

(defun parse-xdebug-trace (stream)
  (read-line stream nil nil) ;the first three
  (read-line stream nil nil) ;lines are really
  (read-line stream nil nil) ;not needed
  (do ((line (read-line stream nil nil)
	     (read-line stream nil nil))
       (records nil)
       (stop-p nil))
      (stop-p (reverse records))
    (let ((last (parse-xdebug-trace-line line)))
      (if last 
	  (push last records)
	  (setf stop-p T)))))
	  

(defun make-xdebug-trace (stream)
  (make-instance 'xdebug-trace
		 :trace-content (parse-xdebug-trace stream)))


(defmethod get-changed-files-paths ((xdebug-trace xdebug-trace))
  (mapcar #'(lambda(fopen-call)
	      (parameters fopen-call))
	  (remove-if-not #'(lambda (record)
			     (and (typep record 'entry-record)
				  (string= (function-name record) "fopen")))
			 (trace-content xdebug-trace))))
