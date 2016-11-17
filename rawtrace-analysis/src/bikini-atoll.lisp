(setf sb-ext:*posix-argv* 
      '("/useless/exec/path"
        "-v" "/home/simkoc/oxid-user1-buy-II-201611162047-vilanoo.db"
        "-m" "/home/simkoc/oxid-user1-buy-II-201611162047-mosgi.db"
        "-d" "/home/simkoc/oxid-user1-buy-II-201611162047-analyzed.db"
        "-S" "/home/simkoc/hiwi/csrf/vilanoo/data/DBSchema.sql"))




(analyzer:main)

(in-package :xdebug)

(defparameter *test*
    (make-xdebug-trace-from-file "/home/simkoc/simple.xt"))


(get-pdo-prepared-queries *test*)
