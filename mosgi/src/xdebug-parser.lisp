(in-package :de.uni-saarland.syssec.mosgi.xdebug)


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
      (cl-ppcre:split "\t" line)
    (ecase always
      ("0"
       (make-entry-records (append (list level function-nr always) rest)))
      ("1"
       (make-exit-records (append (list level function-nr always) rest)))
      ("R"
       (make-return-records (append (list level function-nr always) rest))))))


(defun parse-xdebug-trace (stream)
  (do ((line (read-line stream nil nil)
	     (read-line stream nil nil))
       (records nil))
      ((not line) (reverse records))
    (push (parse-xdebug-trace-line line)
	  records)))


(defun make-xdebug-trace (stream)
  (make-instance 'xdebug-trace
		 :trace-content (parse-xdebug-trace stream)))


