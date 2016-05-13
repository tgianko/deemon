(in-package :de.uni-saarland.syssec.mosgi.communication)


(defclass communication-handler ()
  ((server
    :initarg :server
    :accessor server)
   (connection
    :initarg :connection
    :reader connection)
   (iostream
    :initarg :iostream
    :accessor iostream)))



(defun create-communication-handler (listen-ip listen-port)
  (let ((server (make-instance 'sb-bsd-sockets:inet-socket 
			       :type :stream  
			       :protocol :tcp)))
    (sb-bsd-sockets:socket-bind server (sb-bsd-sockets:make-inet-address listen-ip) listen-port)
    (sb-bsd-sockets:socket-listen server 0) ;only one connection accepted
    (multiple-value-bind (connected-socket peer-address)
	(sb-bsd-sockets:socket-accept server)
      (FORMAT T "Connected to ~a~%" peer-address)
      (make-instance 'communication-handler
		     :server server
		     :connection connected-socket
		     :iostream (sb-bsd-sockets:socket-make-stream connected-socket :input T :output T :buffering :none)))))


(defmethod close-communication-handler ((handler communication-handler))
  (with-slots (server connection)
      handler
    (sb-bsd-sockets:socket-close connection) ;also closes open streams by API
    (sb-bsd-sockets:socket-close server)
    (FORMAT T "Connection to peer closed")))


(defmacro with-connected-communication-handler ((handler listen-ip listen-port) &body body)
  `(let ((,handler (create-communication-handler ,listen-ip ,listen-port)))
	 (unwind-protect 
	      (handler-case
		  (progn 
		    ,@body)
		(error (e)
		  (FORMAT T "ECOUNTERED ERROR ~a~%" e)))	   
       (close-communication-handler ,handler))))
	    
     

(defmethod receive-character ((handler communication-handler))
  (read-char (iostream handler) nil 'eof))


(defmethod send-character ((handler communication-handler) (char character))
  (FORMAT T "~a~%" (stream-element-type (iostream handler)))
  (write-char char (iostream handler))
  (finish-output (iostream handler)))
