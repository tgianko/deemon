#|
Author:Simon Koch <s9sikoch@stud.uni-saarland.de>
This file contains the interface code wrapping the libssh2
library into neat lil' calls which are less verbose and 
handle all the nasty stuff we do not want/need to think about
|#
(in-package :de.uni-saarland.syssec.mosgi.ssh-interface)


(defun run-remote-shell-command (command result-handler username host password)
  "executes a shell command on the given host and gives the resulting stream
to the result handler. The result of the result-handler will be returned"
  (libssh2:with-ssh-connection session (host
					(libssh2:make-password-auth username password)
					:hosts-db (namestring
						   (merge-pathnames
						    (make-pathname :directory '(:relative ".ssh")
								   :name "libssh2-known_hosts")
						    (user-homedir-pathname))))
    (libssh2:with-execute*  (stream session command)
      (funcall result-handler stream))))


(defun scp (guest-file host-file username host password)
  "copies a the guest file to the target file from the given host using
provided password and username with scp"
  (libssh2:with-ssh-connection session (host
					(libssh2:make-password-auth username password)
					:hosts-db (namestring
						   (merge-pathnames
						    (make-pathname :directory '(:relative ".ssh")
								   :name "libssh2-known_hosts")
						    (user-homedir-pathname))))
    (libssh2:scp-get guest-file host-file)))


(defun folder-content-guest (folder username host password)
  "returns a string list with the folder content of the provided folder
path on the given host using password and username to log in"
  (run-remote-shell-command (FORMAT nil "ls -p ~a | grep -v /" folder) host username password
			    #'(lambda(stream)
				(do* ((line (read-line stream nil nil nil)
					    (read-line stream nil nil nil))
				      (files (list line)
					     (cons line files)))
				     ((not line) (cdr files))))))

