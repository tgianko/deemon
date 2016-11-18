(setf sb-ext:*posix-argv* 
      '("/useless/exec/path"
        "-v" "/home/simkoc/prestashop-test-201611171610-vilanoo.db"
        "-m" "/home/simkoc/prestashop-test-201611171610-mosgi.db"
        "-d" "/home/simkoc/prestashop-test-201611171610-analyzed.db"
        "-S" "/home/simkoc/hiwi/csrf/vilanoo/data/DBSchema.sql"))




(analyzer:main)

(in-package :xdebug)

(defparameter *test*
    (make-xdebug-trace-from-file "/home/simkoc/simple.xt"))


(get-pdo-prepared-queries *test*)
