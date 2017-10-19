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

(ql:quickload "analyzer")
(load "./main2.lisp")

(handler-case
    (analyzer:main)
  (sb-sys:interactive-interrupt ()
    (FORMAT T "user invoked shutdown"))
  (error (e)
    (FORMAT T "encountered fatal error '~a'~%" e)))
