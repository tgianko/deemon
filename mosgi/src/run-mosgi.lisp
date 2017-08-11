#|
author: Simon Koch <s9sikoch@stud.uni-saarland.de
This file is wrapper to call/start mosgi via command line and
ensure that any uncaught error is caught and properly displayed
as well as enabling interactive interrupts which would be
uncaught and inconvinient otherwise.
|#
(ql:quickload "mosgi")


(handler-case
    (mosgi:main)
  (sb-sys:interactive-interrupt ()
    (mosgi:print-threaded :mosgi "user invoked shutdown"))
  (error (e)
    (mosgi:print-threaded :mosgi (FORMAT nil "encountered fatal error '~a'~%" e))))
