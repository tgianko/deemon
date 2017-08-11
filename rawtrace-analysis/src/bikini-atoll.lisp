(setf sb-ext:*posix-argv* 
      '("/useless/exec/path"        
        "-m" "/home/simkoc/tmp/mauticbug/mautic-admin_user-delete_contact_S1-201702022125-mosgi.db"
        "-v" "/home/simkoc/tmp/mauticbug/mautic-admin_user-delete_contact_S1-201702022125-vilanoo.db"
        "-d" "/home/simkoc/tmp/mauticbug/mautic-admin_user-delete_contact_S1-201702022125-analyzer.db"
        "-S" "/home/simkoc/hiwi/csrf/vilanoo/data/DBSchema.sql"))
        
(analyzer:main)

(in-package :xdebug)

(defparameter *test*
    (make-xdebug-trace-from-file "/home/simkoc/simple.xt"))


(get-pdo-prepared-queries *test*)
