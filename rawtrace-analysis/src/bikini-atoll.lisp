(setf sb-ext:*posix-argv* 
      '("/useless/exec/path"        
        "-m" "/home/simkoc/hiwi/csrf/tmp/abantecart-login_and_change_email-201612121442-csrftests-mosgi.db"
        "-d" "/home/simkoc/hiwi/csrf/tmp/abantecart-login_and_change_email-analyzed.db"
        "-S" "/home/simkoc/hiwi/csrf/vilanoo/data/DBSchema.sql"
        "-f" "1"
        "-e" "1"))


(analyzer:main)

(in-package :xdebug)

(defparameter *test*
    (make-xdebug-trace-from-file "/home/simkoc/simple.xt"))


(get-pdo-prepared-queries *test*)


#|
(with-open-file (istream "/tmp/analysis-query-result-buffer" :external-format :latin1)
  (with-open-file (ostream "~/test-dump.xt" :direction :output :if-does-not-exist :create :if-exists :supersede)
    (remove-bad-newlines istream ostream)))
|#
