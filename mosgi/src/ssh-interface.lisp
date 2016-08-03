#|
Author:Simon Koch <s9sikoch@stud.uni-saarland.de>
This file contains the interface code wrapping the libssh2
library into neat lil' calls which are less verbose and 
handle all the nasty stuff we do not want/need to think about
|#
(in-package :de.uni-saarland.syssec.mosgi.ssh-interface)

;;YEAH openssh is not thread save -.-
(defparameter *ssh-mutex* (sb-thread:make-mutex :name "ssh-mutex"))

(defun run-remote-shell-command (command username host password result-handler)
  "executes a shell command on the given host and gives the resulting stream
to the result handler. The result of the result-handler will be returned"
  (sb-thread:with-mutex (*ssh-mutex*)
    (libssh2:with-ssh-connection session (host
					  (libssh2:make-password-auth username password)
					  :hosts-db (namestring
						     (merge-pathnames
						      (make-pathname :directory '(:relative ".ssh")
								     :name "libssh2-known_hosts")
						      (user-homedir-pathname))))
      (libssh2:with-execute*  (stream session command)
	(funcall result-handler stream)))))
  


(defun scp (guest-file host-file username host password)
  "copies a the guest file to the target file from the given host using
provided password and username with scp"
  (sb-thread:with-mutex (*ssh-mutex*)
    (libssh2:with-ssh-connection session (host
					  (libssh2:make-password-auth username password)
					  :hosts-db (namestring
						     (merge-pathnames
						      (make-pathname :directory '(:relative ".ssh")
								     :name "libssh2-known_hosts")
						      (user-homedir-pathname))))
      (libssh2:scp-get guest-file host-file))))


(defun folder-content-guest (folder username host password)
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
				     ((not line) (cdr files))))))



(defun convert-to-utf8-encoding (file-path)
  (sb-ext:run-program "/usr/bin/vim" (list "+set nobomb | set fenc=utf8 | x" file-path)))


(defun discard-data-lambda ()
  #'(lambda(stream)
      (do* ((line (read-line stream nil nil nil)
		  (read-line stream nil nil nil)))
	   ((not line)))))


(defun backup-all-files-from (folder target-folder user host pwd)
  (run-remote-shell-command (FORMAT nil "mkdir ~a" target-folder) user host pwd (discard-data-lambda))
  (run-remote-shell-command (FORMAT nil "cp ~a/* ~a" folder target-folder) user host pwd (discard-data-lambda)))


(defun backup-file (file target-folder user host pwd)
  (run-remote-shell-command (FORMAT nil "mkdir ~a" target-folder) user host pwd (discard-data-lambda))
  (run-remote-shell-command (FORMAT nil "cp ~a ~a" file target-folder) user host pwd (discard-data-lambda)))


(defun delete-folder (folder user host pwd)
  (run-remote-shell-command (FORMAT nil "rm -rf ~a" folder) user host pwd (discard-data-lambda)))


(defun probe-machine (username host password)
  (folder-content-guest "/" username host password)
  nil)

#|
  (libssh2:with-ssh-connection session (host 
					(libssh2:make-password-auth username password)
					:hosts-db (namestring
						   (merge-pathnames 
						    (make-pathname :directory '(:relative ".ssh")
								   :name "libss2-known_hosts")
						    (user-homedir-pathname))))|#
			       
			       


(defun register-machine (username host password)  
  (handler-case 
      (handler-bind ((libssh2::ssh-bad-hostkey #'(lambda (e) 
						   (declare (ignore e)) 
						   (FORMAT T "[SSH] register always bad hostkey for given machine~%")
						   (invoke-restart 'libssh2:accept-always)))
		     (libssh2::ssh-unknown-hostkey #'(lambda (e) 
						       (declare (ignore e)) 
						       (FORMAT T "[SSH] register always unknown hostkey for given machine~%")
						       (invoke-restart 'libssh2:accept-always))))
	(probe-machine username host password))
    (libssh2::ssh-authentication-failure (err)
      (declare (ignore err))))
  (probe-machine username host password))
	       

    


