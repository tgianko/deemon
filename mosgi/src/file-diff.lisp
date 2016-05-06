(in-package :de.uni-saarland.syssec.mosgi.file-diff)


(defun change-amount-byte (file-stream-a file-stream-b)
  (- (file-length file-stream-a)
     (file-length file-stream-b)))
