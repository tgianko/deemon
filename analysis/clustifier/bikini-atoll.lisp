(print:dump-results-into-folder
 (database:get-all-requests "/home/simkoc/.vilanoo/abantecart/abantecard-create-account-201607221205.db"
			    "0"
			    :end-id nil)
 "/home/simkoc/tmp/vilanoo-abantecard-create-account/")

(print:dump-results-into-folder
 (database:get-all-requests "/home/simkoc/.vilanoo/abantecart/abantecard-change-mail-201607221211.db"
			    "0"
			    :end-id nil)
 "/home/simkoc/tmp/vilanoo-abantecard-change-email/")


(print:dump-results-into-folder
 (database:get-all-requests "/home/simkoc/.vilanoo/abantecart/abantecard-change-password-201607221348.db"
			    "0"
			    :end-id nil)
 "/home/simkoc/tmp/vilanoo-abantecard-change-pwd/")


(print:dump-results-into-folder
 (database:get-all-requests "/home/simkoc/.vilanoo/opencart/opencart-create-account-201607221357.db"
			    "0"
			    :end-id nil)
 "/home/simkoc/tmp/vilanoo-opencart-create-account/")



(print:dump-results-into-folder
 (database:get-all-requests "/home/simkoc/.vilanoo/opencart/opencart-change-password-201607221359.db"
			    "0"
			    :end-id nil)
 "/home/simkoc/tmp/vilanoo-opencart-change-password/")



(print:dump-results-into-folder
 (database:get-all-requests "/home/simkoc/.vilanoo/opencart/opencart-change-email-201607221408.db"
			    "0"
			    :end-id nil)
 "/home/simkoc/tmp/vilanoo-opencart-change-email/")


