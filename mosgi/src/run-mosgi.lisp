(ql:quickload "mosgi")


(handler-bind 
    ((libssh2:ssh-unknown-hostkey #'(lambda(err)
				      (declare (ignore err))
				      (invoke-restart 'libssh2:accept-always))))
  (mosgi:main))
