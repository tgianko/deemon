(ql:quickload "mosgi")


(handler-case
    (mosgi:main)
  (sb-sys:interactive-interrupt ()
    (mosgi:print-threaded :mosgi "user invoked shutdown"))
  (error (e)
    (mosgi:print-threaded :mosgi (FORMAT nil "encountered fatal error '~a'~%" e))))
