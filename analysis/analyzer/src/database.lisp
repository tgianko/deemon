(in-package :de.uni-saarland.syssec.mosgi.database)


(clsql:file-enable-sql-reader-syntax)


(defmethod commit-full-sessions (database request-db-id php-session-list)
  (dolist (session php-session-list)
    (clsql:insert-records :into [SESSIONS]
			  :ATTRIBUTES '([HTTP-REQUEST-ID] [SESSION-ID] [SESSION-STRING])
			  :VALUES (list request-db-id (php-session:session-id session)
					(FORMAT nil "~a" session))
			  :database database)))

(defmethod commit-latest-diff (database request-db-id (state-trace diff:state-trace))  
  (if (diff:diff-history state-trace)
      (commit-latest-diff database request-db-id (car (diff:diff-history state-trace)))))


(defmethod commit-latest-diff (database request-db-id (fhs diff:file-diff-entry))
  (dolist (diff-entry (diff:file-diff-entry-diffs fhs))
    (clsql:insert-records :into [CHANGED-FILES]
			  :ATTRIBUTES '([HTTP-REQUEST-ID] [FILE-PATH])
			  :VALUES (list request-db-id diff-entry)					
			  :database database)))


(defmethod commit-latest-diff (database request-db-id (shs diff:php-session-diff-entry))
  (dolist (diff-entry (diff:php-session-diff-entry-diffs shs))
    (clsql:insert-records :INTO [SESSION-DIFF-TREES]
			  :ATTRIBUTES '([HTTP-REQUEST-ID]
					[SESSION-ID]
					[DIFF-TREE])
			  :VALUES (list request-db-id (php-session:session-id (car diff-entry)) (FORMAT nil "~a" diff-entry))
			  :database database)))
				      

(defmethod commit-sql-queries (database request-db-id mysql-queries)
  (do ((query mysql-queries (cdr query))
       (counter 0 (+ counter 1)))
      ((not query) nil)
    (clsql:insert-records :INTO [SQL-QUERIES]
			  :ATTRIBUTES '([HTTP-REQUEST-ID]
					[QUERY-COUNTER]
					[QUERY-STRING])
			  :VALUES (list request-db-id counter (car query))
			  :database database)))
