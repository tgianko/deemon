(in-package :de.uni-saarland.syssec.mosgi)


(defconstant +legal-communication-bytes+ '((0 . :START-DIFF) (1 . :KILL-YOURSELF) (2 . :FINISHED-DIFF))



(defun wait-for-order (port)
  )


(defun send-finished-diff-byte (ip port)
  )


(defun make-diff ()
    )


(defun main ()
  (unix-options:with-cli-options ('() T) 
      (&parameters listen-port port ip xdebug-file-path php-session-folder)
    (do ()
	(T nil)
      (let ((order (wait-for-order)))
	(ecase order 
	  (:START-DIFF 
	   (make-diff))
	  (:KILL-YOURSELF
	   (return-from main nil)))
	(send-finished-diff-byte)))))


(unix-options:with-cli-options () 
    (&parameters test test-2)
  (print test)
  (print test-2))



(opts:define-opts
  (:name :test
	 :description "test"
	 :short #\t
	 :long "test")
  (:name :test-2
	 :description "test-2"
	 :short #\T
	 :long "test-2"))


(opts:get-opts)
