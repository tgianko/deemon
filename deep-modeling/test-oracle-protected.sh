#!/bin/bash


folder="/home/simkoc/hiwi/csrf/protected-csrf"


tgen=(\
"abantecart-login_and_buy_stuff-201701251237-tgen-su_uu_var_singleton.db" \
"abantecart-login_and_change_address-201701251237-tgen-su_uu_var_singleton.db" \
"abantecart-login_and_change_order_status-201701251237-tgen-su_uu_var_singleton.db" \
"abantecart-login_and_change_product_price-201701251237-tgen-su_uu_var_singleton.db" \
"abantecart-login_and_change_store_settings-201701251237-tgen-su_uu_var_singleton.db" \
"abantecart-login_and_delete_category-201701251237-tgen-su_uu_var_singleton.db" \
"horde-login_and_add_address_to_whitelist-201701251237-tgen-su_uu_var_singleton.db" \
"horde-login_and_add_user-201701251237-tgen-su_uu_var_singleton.db" \
"horde-login_and_delete_table-201701251237-tgen-su_uu_var_singleton.db" \
"invoiceninja-add_new_default_tax_rate-201701251237-tgen-su_uu_var_singleton.db" \
"invoiceninja-add_payment-201701251237-tgen-su_uu_var_singleton.db" \
"invoiceninja-change_smtp_config-201701251237-tgen-su_uu_var_singleton.db" \
"invoiceninja-create_client-201701251237-tgen-su_uu_var_singleton.db" \
"invoiceninja-create_expense-201701251237-tgen-su_uu_var_singleton.db" \
"invoiceninja-create_invoice-201701251237-tgen-su_uu_var_singleton.db" \
"invoiceninja-create_new_user-201701251237-tgen-su_uu_var_singleton.db" \
"invoiceninja-create_task-201701251237-tgen-su_uu_var_singleton.db" \
"invoiceninja-delete_client-201701251237-tgen-su_uu_var_singleton.db" \
"invoiceninja-deletes_task-201701251237-tgen-su_uu_var_singleton.db" \
"invoiceninja-update_client-201701251237-tgen-su_uu_var_singleton.db" \
"mautic-change_landing_page-201702031709-tgen-su_uu_var_singleton.db" \
"mautic-change_password-201702031709-tgen-su_uu_var_singleton.db" \
"mautic-create_contact-201702031709-tgen-su_uu_var_singleton.db" \
"mautic-delete_campaign-201702031709-tgen-su_uu_var_singleton.db" \
"mautic-delete_contact-201702031709-tgen-su_uu_var_singleton.db" \
"mybb-create_modify_delete_post-201701251237-tgen-su_uu_var_singleton.db" \
"opencart-login_and_buy_stuff-201701251237-tgen-su_uu_var_singleton.db" \
"opencart-login_and_change_address-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-buy_stuff-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-create_category-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-create_discount-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-create_discount_series-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-create_product-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-create_shipping_rule-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-create_user-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-logs_in_and_buys_stuff-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-logs_in_and_changes_email-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-logs_in_and_changes_password-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-run_sql-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-update_category-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-update_product-201701251237-tgen-su_uu_var_singleton.db" \
"oxid-update_shipping_rule-201701251237-tgen-su_uu_var_singleton.db" \
"prestashop-create_category-201701251237-tgen-su_uu_var_singleton.db" \
"prestashop-create_product-201701251237-tgen-su_uu_var_singleton.db" \
"prestashop-disable_module-201701251237-tgen-su_uu_var_singleton.db" \
"prestashop-disable_some_prefs-201701251237-tgen-su_uu_var_singleton.db" \
"prestashop-erase_logs-201701251237-tgen-su_uu_var_singleton.db" \
"prestashop-log_in_buy_stuff-201701251237-tgen-su_uu_var_singleton.db" \
"prestashop-log_in_change_email-201701251237-tgen-su_uu_var_singleton.db" \
"prestashop-log_in_create_new_address-201701251237-tgen-su_uu_var_singleton.db" \
"prestashop-toggle_demomode-201701251237-tgen-su_uu_var_singleton.db" \
"simplemachinesforum-change_some_sec_prefs-201701251237-tgen-su_uu_var_singleton.db" \
"simplemachinesforum-create_new_board-201701251237-tgen-su_uu_var_singleton.db" \
"simplemachinesforum-create_topic-201701251237-tgen-su_uu_var_singleton.db" \
"simplemachinesforum-mark_as_read-201701251237-tgen-su_uu_var_singleton.db" \
"simplemachinesforum-modify_post-201701251237-tgen-su_uu_var_singleton.db" \
"simplemachinesforum-reply-201701251237-tgen-su_uu_var_singleton.db" \
"simplemachinesforum-upgrade_account-201701251237-tgen-su_uu_var_singleton.db" \
)

analyzed=(\
"abantecart-login_and_buy_stuff-201701251244-csrftests-analyzed.db" \
"abantecart-login_and_change_address-201701251245-csrftests-analyzed.db" \
"abantecart-login_and_change_order_status-201701251239-csrftests-analyzed.db" \
"abantecart-login_and_change_product_price-201701251240-csrftests-analyzed.db" \
"abantecart-login_and_change_store_settings-201701251242-csrftests-analyzed.db" \
"abantecart-login_and_delete_category-201701251242-csrftests-analyzed.db" \
"horde-login_and_add_address_to_whitelist-201701251513-csrftests-analyzed.db" \
"horde-login_and_add_user-201701251510-csrftests-analyzed.db" \
"horde-login_and_delete_table-201701251512-csrftests-analyzed.db" \
"invoiceninja-add_new_default_tax_rate-201701251437-csrftests-analyzed.db" \
"invoiceninja-add_payment-201701251434-csrftests-analyzed.db" \
"invoiceninja-change_smtp_config-201701251439-csrftests-analyzed.db" \
"invoiceninja-create_client-201701251424-csrftests-analyzed.db" \
"invoiceninja-create_expense-201701251430-csrftests-analyzed.db" \
"invoiceninja-create_invoice-201701251432-csrftests-analyzed.db" \
"invoiceninja-create_new_user-201701251436-csrftests-analyzed.db" \
"invoiceninja-create_task-201701251428-csrftests-analyzed.db" \
"invoiceninja-delete_client-201701251425-csrftests-analyzed.db" \
"invoiceninja-deletes_task-201701251440-csrftests-analyzed.db" \
"invoiceninja-update_client-201701251427-csrftests-analyzed.db" \
"mautic-change_landing_page-201702031740-csrftests-analyzed.db" \
"mautic-change_password-201702031742-csrftests-analyzed.db" \
"mautic-create_contact-201702031733-csrftests-analyzed.db" \
"mautic-delete_campaign-201702031738-csrftests-analyzed.db" \
"mautic-delete_contact-201702031737-csrftests-analyzed.db" \
"mybb-create_modify_delete_post-201701251441-csrftests-analyzed.db" \
"opencart-login_and_buy_stuff-201701251246-csrftests-analyzed.db" \
"opencart-login_and_change_address-201701251247-csrftests-analyzed.db" \
"oxid-buy_stuff-201701251319-csrftests-analyzed.db" \
"oxid-create_category-201701251248-csrftests-analyzed.db" \
"oxid-create_discount-201701251249-csrftests-analyzed.db" \
"oxid-create_discount_series-201701251256-csrftests-analyzed.db" \
"oxid-create_product-201701251258-csrftests-analyzed.db" \
"oxid-create_shipping_rule-201701251301-csrftests-analyzed.db" \
"oxid-create_user-201701251308-csrftests-analyzed.db" \
"oxid-logs_in_and_buys_stuff-201701251352-csrftests-analyzed.db" \
"oxid-logs_in_and_changes_email-201701251326-csrftests-analyzed.db" \
"oxid-logs_in_and_changes_password-201701251325-csrftests-analyzed.db" \
"oxid-run_sql-201701251311-csrftests-analyzed.db" \
"oxid-update_category-201701251312-csrftests-analyzed.db" \
"oxid-update_product-201701251313-csrftests-analyzed.db" \
"oxid-update_shipping_rule-201701251316-csrftests-analyzed.db" \
"prestashop-create_category-201701251354-csrftests-analyzed.db" \
"prestashop-create_product-201701251355-csrftests-analyzed.db" \
"prestashop-disable_module-201701251356-csrftests-analyzed.db" \
"prestashop-disable_some_prefs-201701251357-csrftests-analyzed.db" \
"prestashop-erase_logs-201701251357-csrftests-analyzed.db" \
"prestashop-log_in_buy_stuff-201701251418-csrftests-analyzed.db" \
"prestashop-log_in_change_email-201701251359-csrftests-analyzed.db" \
"prestashop-log_in_create_new_address-201701251411-csrftests-analyzed.db" \
"prestashop-toggle_demomode-201701251358-csrftests-analyzed.db" \
"simplemachinesforum-change_some_sec_prefs-201701251455-csrftests-analyzed.db" \
"simplemachinesforum-create_new_board-201701251450-csrftests-analyzed.db" \
"simplemachinesforum-create_topic-201701251442-csrftests-analyzed.db" \
"simplemachinesforum-mark_as_read-201701251444-csrftests-analyzed.db" \
"simplemachinesforum-modify_post-201701251448-csrftests-analyzed.db" \
"simplemachinesforum-reply-201701251446-csrftests-analyzed.db" \
"simplemachinesforum-upgrade_account-201701251458-csrftests-analyzed.db" \
)



function create_csv_listing_but {
    local -n arr=$1
    local ret=""
    for loc in "${!arr[@]}";
    do
        if [ "${loc}" !=  "${2}" ]; then
            local ret="${ret}${folder}/${arr[$loc]},"
        fi
    done
    echo "${ret::-1}"
}



function test {
    for i in "${!tgen[@]}";
    do
        echo ">>"
        echo $i
        echo "${tgen[$i]}"
        echo "${analyzed[$i]}"
    done
}


for i in "${!tgen[@]}";
do
    tc_references=`create_csv_listing_but tgen ${i}`
    tc_references_analyzed=`create_csv_listing_but analyzed ${i}`
    tc=${tgen[$i]}
    tc_analyzed=${analyzed[$i]}
    echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
    # echo ./testermanager.py oracle ${tc_references} ${tc_references_analyzed} "${folder}/${tc}" "${folder}/${tc_analyzed}"
    ./testermanager.py oracle ${tc_references} ${tc_references_analyzed} "${folder}/${tc}" "${folder}/${tc_analyzed}"
    echo "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
done
