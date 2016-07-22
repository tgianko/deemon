(in-package :de.uni-saarland.syssec.clustifier.database)


(clsql:file-enable-sql-reader-syntax)


(defparameter *path-to-query-normalizer* "/home/simkoc/hiwi/csrf/vilanoo/analysis/clustifier/sql_query_normalization.py")
      

(defun hash-sha256-string (string)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence 
    :sha256
    (ironclad:ascii-string-to-byte-array string))))


(defclass http-request ()
  ((http-request-id 
    :initarg :http-request-id
    :reader http-request-id)
   (timestamp
    :initarg :timestamp
    :reader timestamp)
   (request-url
    :initarg :request-url
    :reader request-url)
   (request-body 
    :initarg :request-body
    :reader request-body)
   (header
    :initarg :request-header
    :reader header)
   (method-type
    :initarg :methode-type
    :reader method-type)
   (cookies
    :initarg :cookies
    :reader cookies)
   (status-code
    :initarg :status-code
    :reader status-code)
   (sql-queries 
    :initarg :sql-queries
    :reader sql-queries)
   (sql-hash
    :reader sql-hash)
   (changed-files 
    :initarg :changed-files
    :reader changed-files)
   (file-hash
    :reader file-hash)
   (session-diffs
    :initarg :session-diffs
    :reader session-diffs)
   (session-hash
    :reader session-hash)))


(defmethod print-object ((hr http-request) stream)
  (with-slots (http-request-id timestamp request-url request-body header method-type 
			       cookies status-code sql-queries sql-hash changed-files file-hash 
			       session-diffs session-hash)
      hr
    (FORMAT stream "<~a,~a,~a>~%" sql-hash file-hash session-hash)
    (FORMAT stream "ID:~a TS:~a~%" http-request-id timestamp)
    (FORMAT stream "URL:~a~%" request-url)
    (FORMAT stream "METHOD:~a~%" method-type)
    (FORMAT stream "STATUS:~a~%" status-code)
    (FORMAT stream "HEADER:~a~%" header)
    (FORMAT stream "BODY:~a~%" request-body)
    (FORMAT stream "COOKIES:~a~%" cookies)
    (FORMAT stream "QUERY-HASH:~a~%" sql-hash)
    (FORMAT stream "~{~a~%~}" sql-queries)
    (FORMAT stream "FILE-HASH:~a~%" file-hash)
    (FORMAT stream "~a~%" changed-files)
    (FORMAT stream "SESSION-HASH:~a~%" session-hash)
    (FORMAT stream "~{~a~%~}" session-diffs)))


(defmethod initialize-instance :after ((hr http-request) &rest stuff)
  (declare (ignore stuff))
  (setf (slot-value hr 'sql-hash)
	(hash-sha256-string (apply 'concatenate 'string 
				   (sort (mapcar #'query-hash (sql-queries hr)) #'string<=))))
  (setf (slot-value hr 'file-hash)
	(file-hash (changed-files hr)))
  (setf (slot-value hr 'session-hash)
	(hash-sha256-string (apply 'concatenate 'string 
				   (sort (mapcar #'session-diff-hash (session-diffs hr)) #'string<=)))))
  

(defclass sql-query ()
  ((query-string
    :initarg :query-string
    :reader query-string)
   (query-hash
    :reader query-hash)))


(defmethod print-object ((sq sql-query) stream)
  (FORMAT stream "~a => ~a" (query-hash sq) (query-string sq)))


(defun get-query-hash (string)
  (let ((stream (sb-ext:process-output (sb-ext:run-program "/usr/bin/python" (list *path-to-query-normalizer* string) :output :stream))))
    (unwind-protect
	 (read-line stream)
      (close stream))))    


(defmethod initialize-instance :after ((sq sql-query) &rest stuff)
  (declare (ignore stuff))
  (setf (slot-value sq 'query-hash)
	(get-query-hash (slot-value sq 'query-string))))


(defclass changed-files-list ()
  ((changed-files-list
    :initarg :changed-files-list
    :reader changed-files-list)
   (files-hash
    :reader file-hash)))


(defmethod print-object ((cfl changed-files-list) stream)
  (FORMAT stream "~a => ~{~a~^,~}" (file-hash cfl) (changed-files-list cfl)))


(defmethod initialize-instance :after ((cfl changed-files-list) &rest stuff)
  (declare (ignore stuff))
  (setf (slot-value cfl 'files-hash) 
	(hash-sha256-string (apply 'concatenate 'string 
				   (sort (slot-value cfl 'changed-files-list) #'string<=)))))
  

(defclass session-diff ()
  ((session-id
    :initarg :session-id
    :reader session-id)
   (session-diff-tree
    :initarg :session-diff-tree
    :reader session-diff-tree)
   (session-diff-hash
    :reader session-diff-hash)))


(defmethod print-object ((sd session-diff) stream)
  (FORMAT stream "~a => ~a : ~a" (session-diff-hash sd) (session-id sd) (session-diff-tree sd)))


(defmethod initialize-instance :after ((sd session-diff) &rest stuff)
  (declare (ignore stuff))
  (setf (slot-value sd 'session-diff-hash)
	(hash-sha256-string (slot-value sd 'session-diff-tree))))

    
(defun get-all-sql-queries (http-request-id db-connection)
  (mapcar #'(lambda(tuple)
	      (destructuring-bind (query-string)
		  tuple
		(make-instance 'sql-query
			       :query-string query-string)))
	  (clsql:select [query-string]
			:FROM [sql-queries]
			:WHERE [= [http-request-id] http-request-id]
			:database db-connection)))


(defun get-all-changed-files (http-request-id db-connection)
  (make-instance 'changed-files-list 
		 :changed-files-list (mapcar #'car 
					     (clsql:select [file-path]
							   :FROM [changed-files]
							   :WHERE [= [http-request-id] http-request-id]
							   :database db-connection))))


(defun get-all-session-diffs (http-request-id db-connection)
  (mapcar #'(lambda(tuple)
	      (destructuring-bind (session-id diff-tree)
		  tuple
		(make-instance 'session-diff
			       :session-id session-id
			       :session-diff-tree diff-tree)))
	  (clsql:select [session-id] [diff-tree]
			:FROM [session-diff-trees]
			:WHERE [= [http-request-id] http-request-id]
			:database db-connection)))


(defun http-request-generator (tuple-list db-connection)
  (destructuring-bind (id time request-url request-body header method-type cookies status-code)
      tuple-list
    (make-instance 'http-request
     :http-request-id id
     :timestamp time
     :request-url request-url
     :request-body request-body
     :request-header header 
     :methode-type method-type 
     :cookies cookies 
     :status-code status-code
     :sql-queries (get-all-sql-queries id db-connection)
     :changed-files (get-all-changed-files id db-connection)
     :session-diffs (get-all-session-diffs id db-connection))))


(defun get-all-requests (database-path start-id &key end-id)
  (clsql:with-database (db (list database-path) :database-type :sqlite3)
    (mapcar #'(lambda(tuple) (http-request-generator tuple db))
	    (clsql:select [id] [time] [request-url] [request-body] [header] [method-type] [cookies] [status-code]
			  :FROM [http-requests]		      
			  :WHERE (if end-id
				     [AND [>= [ID] start-id]
				          [<  [ID] end-id  ]]
				     [>= [ID] start-id])
			  :database db))))
				 
