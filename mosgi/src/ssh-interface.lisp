#|
Author:Simon Koch <s9sikoch@stud.uni-saarland.de>
This file contains the interface code wrapping the libssh2
library into neat lil' calls which are less verbose and 
handle all the nasty stuff we do not want/need to think about
|#
(in-package :de.uni-saarland.syssec.mosgi.ssh-interface)

;;YEAH openssh is not thread save -.-
(defparameter *ssh-mutex* (sb-thread:make-mutex :name "ssh-mutex"))

(defparameter *ssh-file-mutex* (sb-thread:make-mutex :name "ssh-mutex"))	    

(defparameter +scp-buffer-file-path+ "/tmp/trival-ssh-buffer-file")

(defparameter *global-ssh-connection* nil)

(defmacro with-active-ssh-connection ((username host password) &body body)
  (let ((connection (gensym)))
    `(unwind-protect
          (ssh:with-connection (,connection ,host (ssh:pass ,username ,password))
            (setf ssh-interface:*global-ssh-connection* ,connection)
            ,@body)
       (setf ssh-interface:*global-ssh-connection* nil))))
         
       
(defun run-remote-shell-command (command username host password result-handler &optional (logger #'(lambda(string)
                                                                                 (FORMAT (make-broadcast-stream) "~a~%" string))))
  "executes a shell command on the given host and gives the resulting stream
to the result handler. The result of the result-handler will be returned"  
  (sb-thread:with-mutex (*ssh-mutex*)
    (unwind-protect
         (progn 
           (funcall logger (FORMAT nil "running remote command ~a" command))
           (ssh:with-command (*global-ssh-connection* stream command)        
             (funcall result-handler stream)))
      (funcall logger (FORMAT nil "finished remote command ~a" command)))))
  

(defun scp (remote-name local-name user host password &optional (logger #'(lambda(string)
                                                                            (FORMAT (make-broadcast-stream) "~a~%" string))))
  (sb-thread:with-mutex (*ssh-mutex*)
    (funcall logger (FORMAT nil "ssh-session ~a - about to start scp for ~a -> ~a" 
                            *global-ssh-connection*
                            remote-name
                            local-name))
    (libssh2:with-scp-input (in *global-ssh-connection* remote-name stat)
      (with-open-file (out local-name
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :element-type '(unsigned-byte 8))
        (cl-fad:copy-stream in out)))))


(defun folder-content-guest (folder username host password &optional (logger #'(lambda(string)
                                                                                 (FORMAT (make-broadcast-stream) "~a~%" string))))
  "returns a string list with the folder content of the provided folder
path on the given host using password and username to log in"
  (when (not (char= #\/ (car (last (coerce folder 'list)))))
    (error 'simple-error
	   :format-control "expected / terminated folder path"))
  (run-remote-shell-command (FORMAT nil "ls -p ~a | grep -v /" folder) username host password
			    #'(lambda(stream)
				(do* ((line (read-line stream nil nil nil)
					    (read-line stream nil nil nil))
				      (files (list (FORMAT nil "~a~a" folder line))
					     (cons (FORMAT nil "~a~a" folder line) files)))
				     ((not line) (cdr files))))
                            logger))


(defun get-all-contained-files-as-strings (folder-path username host password &optional (logger #'(lambda(string)
                                                                                                    (FORMAT (make-broadcast-stream) "~a~%" string))))
  "returns a list of strings that represent the content of the files contained
in folder path using ssh connection with provided username host and password"
  (let ((folder-files (folder-content-guest folder-path username host password)))
    (funcall logger (FORMAT nil "scanned folder ~a and found ~a files" folder-path (length folder-files)))
    (mapcar #'(lambda(file-path)
                (cons file-path (get-file-as-string file-path username host password logger)))
            folder-files)))


(defun get-file-as-string (file-path username host password &optional (logger #'(lambda(string)
                                                                                  (FORMAT (make-broadcast-stream) "~a~%" string))))
  "returns the string that represents the contetn of the file specified as file-path
using ssh connection with provided username host and password"
  (sb-thread:with-mutex (*ssh-file-mutex*)
    (sb-ext:gc :full t)
    (scp file-path +scp-buffer-file-path+ username host password)
    (let ((result (get-file-as-simple-string +scp-buffer-file-path+)))
      (sleep 3)
      (sb-ext:gc :full t)
      result))) ;I MUST NOT DELETE SHIT FROM INSIDE A FILE


(defun get-file-as-file (remote-target local-target &optional (logger #'(lambda(string)
                                                                          (FORMAT (make-broadcast-stream) "~a~%" string))))
  (scp remote-target local-target "" "" "" logger)
  local-target)


(defun get-file-as-simple-string (file-path)
  (labels ((get-simple-string ()
             (with-open-file (tmp-stream file-path :element-type 'character :external-format :latin1)
               (let ((sequence (make-array (file-length tmp-stream) :element-type 'character :adjustable nil)))
                 (read-sequence sequence tmp-stream)
                 (let ((clean-sequence (map 'list #'(lambda(char)
                                                      (if (not (typep char 'base-char))
                                                          #\SPACE
                                                          char)) sequence)))
                   (let ((simple-string (make-array (length clean-sequence) 
                                                    :element-type 'base-char
                                                    :initial-contents clean-sequence
                                                    :adjustable nil)))
                     (setf sequence nil)
                     (setf clean-sequence nil)
                     simple-string))))))
    (let ((simple-string (get-simple-string)))
      simple-string)))


(defun get-file-as-blob (file-path username host password &optional (logger #'(lambda(string)
                                                                                (FORMAT (make-broadcast-stream) "~a~%" string))))
  "returns the binary data that represents the content of the file specified as file-path
using ssh connection with provided username host and password"
  (sb-thread:with-mutex (*ssh-file-mutex*)
    (scp file-path +scp-buffer-file-path+ username host password)  
    (with-open-file (tmp-stream +scp-buffer-file-path+ :element-type '(unsigned-byte 8))
      (funcall logger (FORMAT nil "transfered ~a bytes from ~a" (file-length tmp-stream) file-path))
      (let ((sequence (make-array (file-length tmp-stream) :element-type '(unsigned-byte 8) :adjustable nil)))
        (read-sequence sequence tmp-stream)
        sequence))))


(defun discard-data-lambda ()
  #'(lambda(stream)
      (do* ((line (read-line stream nil nil nil)
		  (read-line stream nil nil nil)))
	   ((not line)))))


(defun backup-all-files-from (folder target-folder user host pwd &optional (logger #'(lambda(string)
                                                                                       (FORMAT (make-broadcast-stream) "~a~%" string))))
  (run-remote-shell-command (FORMAT nil "mkdir ~a" target-folder) user host pwd (discard-data-lambda) logger)
  (run-remote-shell-command (FORMAT nil "cp ~a/* ~a" folder target-folder) user host pwd (discard-data-lambda) logger))


(defun backup-file (file target-folder user host pwd &optional (logger #'(lambda(string)
                                                                           (FORMAT (make-broadcast-stream) "~a~%" string))))
  (run-remote-shell-command (FORMAT nil "mkdir ~a" target-folder) user host pwd (discard-data-lambda) logger)
  (run-remote-shell-command (FORMAT nil "cp ~a ~a" file target-folder) user host pwd (discard-data-lambda) logger))


(defun move-file (file target-folder user host pwd &optional (logger #'(lambda(string)
                                                                           (FORMAT (make-broadcast-stream) "~a~%" string))))
  (run-remote-shell-command (FORMAT nil "mkdir ~a" target-folder) user host pwd (discard-data-lambda) logger)
  (run-remote-shell-command (FORMAT nil "mv ~a ~a" file target-folder) user host pwd (discard-data-lambda) logger))



(defun delete-folder (folder user host pwd &optional (logger #'(lambda(string)
                                                                 (FORMAT (make-broadcast-stream) "~a~%" string))))
  (run-remote-shell-command (FORMAT nil "rm -rf ~a" folder) user host pwd (discard-data-lambda) logger))

