#|
Author:Simon Koch <s9sikoch@stud.uni-saarland.de>
This file provides the interface for communication
with the proxy. It creates a communication handler
which provides an interface for receiving and sending
single chars which represent order/answer codes
|#
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


(defun byte-array->integer (byte-array)
  "converts a given byte array in order of high to low into an
integer"
  (do ((integer 0)
       (counter 0 (+ 1 counter))
       (rem-bytes (reverse byte-array) (cdr rem-bytes)))
      ((not rem-bytes) integer)
    (FORMAT T "setting byte ~a~%" (car rem-bytes))
    (setf integer (dpb (car rem-bytes) (byte 8 (* counter 8)) integer))))


(defmethod receive-nbyte-number ((handler communication-handler) &optional (bytes-count 4)) ;receives number in bytes - default 32b number
  (let ((collection nil))
    (dotimes (i bytes-count)
      (push (read-byte (iostream handler) nil 'eof) collection))
    (byte-array->integer collection)))
    

(defmethod receive-32b-unsigned-integer ((handler communication-handler))
  (receive-nbyte-number handler 4))


(defmethod receive-64b-unsigned-integer ((handler communication-handler))
  (receive-nbyte-number handler 8))


(defmethod send-character ((handler communication-handler) (char character))
  (FORMAT T "~a~%" (stream-element-type (iostream handler)))
  (write-char char (iostream handler))
  (finish-output (iostream handler)))


	 


      
