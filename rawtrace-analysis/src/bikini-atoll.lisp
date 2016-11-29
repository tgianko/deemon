(setf sb-ext:*posix-argv* 
      '("/useless/exec/path"
        "-v" "/home/simkoc/hiwi/csrf/poc-smf-admin-user-201611251425-vilanoo.db"
        "-m" "/home/simkoc/hiwi/csrf/poc-smf-admin-user-201611251425-mosgi.db"
        "-d" "/home/simkoc/hiwi/csrf/poc-smf-admin-user-201611251425-analyzed.db"
        "-S" "/home/simkoc/hiwi/csrf/vilanoo/data/DBSchema.sql"))




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
