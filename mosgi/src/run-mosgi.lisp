; This file is part of Deemon.

; Deemon is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; Deemon is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with Deemon.  If not, see <http://www.gnu.org/licenses/>.

#|
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
