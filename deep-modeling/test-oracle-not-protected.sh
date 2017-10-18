#!/bin/bash
# This file is part of Deemon.

# Deemon is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# Deemon is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Deemon.  If not, see <http://www.gnu.org/licenses/>.


folder="/home/simkoc/hiwi/csrf/not-protected-csrf"


tgen=(\
"abantecart-login_and_add_new_address-201701241619-tgen-not_protected.db" \
"abantecart-login_and_add_second_admin-201701241619-tgen-not_protected.db" \
"abantecart-login_and_buy_stuff-201701241619-tgen-not_protected.db" \
"abantecart-login_and_change_address-201701241619-tgen-not_protected.db" \
"abantecart-login_and_change_email-201701241619-tgen-not_protected.db" \
"abantecart-login_and_change_order_status-201701241619-tgen-not_protected.db" \
"abantecart-login_and_change_password-201701241619-tgen-not_protected.db" \
"abantecart-login_and_change_product_price-201701241619-tgen-not_protected.db" \
"abantecart-login_and_change_store_settings-201701241619-tgen-not_protected.db" \
"abantecart-login_and_delete_category-201701241619-tgen-not_protected.db" \
"horde-login_and_add_address_to_whitelist-201701241619-tgen-not_protected.db" \
"horde-login_and_add_user-201701241619-tgen-not_protected.db" \
"horde-login_and_delete_table-201701241619-tgen-not_protected.db" \
"mautic-change_landing_page-201701241619-tgen-not_protected.db" \
"mautic-change_password-201701241619-tgen-not_protected.db" \
"mautic-create_contact-201701241619-tgen-not_protected.db" \
"mautic-delete_campaign-201701241619-tgen-not_protected.db" \
"mautic-delete_contact-201701241619-tgen-not_protected.db" \
"mautic-embed_analytics-201701241619-tgen-not_protected.db" \
"mybb-add_word_to_filter-201701241619-tgen-not_protected.db" \
"mybb-change_gravatar-201701241619-tgen-not_protected.db" \
"mybb-create_forum_announcement-201701241619-tgen-not_protected.db" \
"mybb-create_modify_delete_post-201701241619-tgen-not_protected.db" \
"mybb-create_new_admin-201701241619-tgen-not_protected.db" \
"mybb-delete_forum-201701241619-tgen-not_protected.db" \
"mybb-delete_user-201701241619-tgen-not_protected.db" \
"mybb-make_user_admin-201701241619-tgen-not_protected.db" \
"mybb-modify_foreign_post-201701241619-tgen-not_protected.db" \
"mybb-modify_theme-201701241619-tgen-not_protected.db" \
"mybb-send_mass_mail-201701241619-tgen-not_protected.db" \
"mybb-turn_offline-201701241619-tgen-not_protected.db" \
"opencart-login_and_add_new_address-201701241619-tgen-not_protected.db" \
"opencart-login_and_buy_stuff-201701241619-tgen-not_protected.db" \
"opencart-login_and_change_address-201701241619-tgen-not_protected.db" \
"opencart-login_and_change_email-201701241619-tgen-not_protected.db" \
"opencart-login_and_change_password-201701241619-tgen-not_protected.db" \
"opencart-login_and_change_product_price-201701241619-tgen-not_protected.db" \
"opencart-login_and_change_store_settings-201701241619-tgen-not_protected.db" \
"opencart-login_and_remane_category-201701241619-tgen-not_protected.db" \
"oxid-buy_stuff-201701241619-tgen-not_protected.db" \
"oxid-create_category-201701241619-tgen-not_protected.db" \
"oxid-create_discount-201701241619-tgen-not_protected.db" \
"oxid-create_discount_series-201701241619-tgen-not_protected.db" \
"oxid-create_product-201701241619-tgen-not_protected.db" \
"oxid-create_shipping_rule-201701241619-tgen-not_protected.db" \
"oxid-create_user-201701241619-tgen-not_protected.db" \
"oxid-logs_in_and_buys_stuff-201701241619-tgen-not_protected.db" \
"oxid-logs_in_and_changes_email-201701241619-tgen-not_protected.db" \
"oxid-logs_in_and_changes_password-201701241619-tgen-not_protected.db" \
"oxid-run_sql-201701241619-tgen-not_protected.db" \
"oxid-update_category-201701241619-tgen-not_protected.db" \
"oxid-update_product-201701241619-tgen-not_protected.db" \
"oxid-update_shipping_rule-201701241619-tgen-not_protected.db" \
"prestashop-create_category-201701241619-tgen-not_protected.db" \
"prestashop-create_customer-201701241619-tgen-not_protected.db" \
"prestashop-create_discount-201701241619-tgen-not_protected.db" \
"prestashop-create_product-201701241619-tgen-not_protected.db" \
"prestashop-delete_carrier-201701241619-tgen-not_protected.db" \
"prestashop-disable_currencies-201701241619-tgen-not_protected.db" \
"prestashop-disable_module-201701241619-tgen-not_protected.db" \
"prestashop-disable_some_prefs-201701241619-tgen-not_protected.db" \
"prestashop-erase_logs-201701241619-tgen-not_protected.db" \
"prestashop-log_in_buy_stuff-201701241619-tgen-not_protected.db" \
"prestashop-log_in_change_email-201701241619-tgen-not_protected.db" \
"prestashop-log_in_create_new_address-201701241619-tgen-not_protected.db" \
"prestashop-toggle_demomode-201701241619-tgen-not_protected.db" \
"simpleinvoice-login_and_add_second_admin-201701241619-tgen-not_protected.db" \
"simpleinvoice-login_and_change_default_tax-201701241619-tgen-not_protected.db" \
"simpleinvoice-login_and_change_tax_rate-201701241619-tgen-not_protected.db" \
"simpleinvoice-login_and_enable_paypal-201701241619-tgen-not_protected.db" \
"simpleinvoice-login_and_new_customer-201701241619-tgen-not_protected.db" \
"simpleinvoice-login_and_new_invoice-201701241619-tgen-not_protected.db" \
"simplemachinesforum-change_some_sec_prefs-201701241619-tgen-not_protected.db" \
"simplemachinesforum-create_new_board-201701241619-tgen-not_protected.db" \
"simplemachinesforum-create_topic-201701241619-tgen-not_protected.db" \
"simplemachinesforum-mark_as_read-201701241619-tgen-not_protected.db" \
"simplemachinesforum-modify_post-201701241619-tgen-not_protected.db" \
"simplemachinesforum-reply-201701241619-tgen-not_protected.db" \
"simplemachinesforum-upgrade_account-201701241619-tgen-not_protected.db" \
)

analyzed=(\
"abantecart-login_and_add_new_address-201701241631-csrftests-analyzed.db" \
"abantecart-login_and_add_second_admin-201701241635-csrftests-analyzed.db" \
"abantecart-login_and_buy_stuff-201701241624-csrftests-analyzed.db" \
"abantecart-login_and_change_address-201701241629-csrftests-analyzed.db" \
"abantecart-login_and_change_email-201701241625-csrftests-analyzed.db" \
"abantecart-login_and_change_order_status-201701241621-csrftests-analyzed.db" \
"abantecart-login_and_change_password-201701241628-csrftests-analyzed.db" \
"abantecart-login_and_change_product_price-201701241621-csrftests-analyzed.db" \
"abantecart-login_and_change_store_settings-201701241622-csrftests-analyzed.db" \
"abantecart-login_and_delete_category-201701241623-csrftests-analyzed.db" \
"horde-login_and_add_address_to_whitelist-201701241853-csrftests-analyzed.db" \
"horde-login_and_add_user-201701241850-csrftests-analyzed.db" \
"horde-login_and_delete_table-201701241852-csrftests-analyzed.db" \
"mautic-change_landing_page-201701241848-csrftests-analyzed.db" \
"mautic-change_password-201701241848-csrftests-analyzed.db" \
"mautic-create_contact-201701241844-csrftests-analyzed.db" \
"mautic-delete_campaign-201701241847-csrftests-analyzed.db" \
"mautic-delete_contact-201701241846-csrftests-analyzed.db" \
"mautic-embed_analytics-201701241849-csrftests-analyzed.db" \
"mybb-add_word_to_filter-201701241741-csrftests-analyzed.db" \
"mybb-change_gravatar-201701241758-csrftests-analyzed.db" \
"mybb-create_forum_announcement-201701241739-csrftests-analyzed.db" \
"mybb-create_modify_delete_post-201701241752-csrftests-analyzed.db" \
"mybb-create_new_admin-201701241737-csrftests-analyzed.db" \
"mybb-delete_forum-201701241745-csrftests-analyzed.db" \
"mybb-delete_user-201701241736-csrftests-analyzed.db" \
"mybb-make_user_admin-201701241731-csrftests-analyzed.db" \
"mybb-modify_foreign_post-201701241747-csrftests-analyzed.db" \
"mybb-modify_theme-201701241743-csrftests-analyzed.db" \
"mybb-send_mass_mail-201701241733-csrftests-analyzed.db" \
"mybb-turn_offline-201701241729-csrftests-analyzed.db" \
"opencart-login_and_add_new_address-201701241648-csrftests-analyzed.db" \
"opencart-login_and_buy_stuff-201701241640-csrftests-analyzed.db" \
"opencart-login_and_change_address-201701241647-csrftests-analyzed.db" \
"opencart-login_and_change_email-201701241644-csrftests-analyzed.db" \
"opencart-login_and_change_password-201701241646-csrftests-analyzed.db" \
"opencart-login_and_change_product_price-201701241637-csrftests-analyzed.db" \
"opencart-login_and_change_store_settings-201701241638-csrftests-analyzed.db" \
"opencart-login_and_remane_category-201701241639-csrftests-analyzed.db" \
"oxid-buy_stuff-201701241657-csrftests-analyzed.db" \
"oxid-create_category-201701241650-csrftests-analyzed.db" \
"oxid-create_discount-201701241651-csrftests-analyzed.db" \
"oxid-create_discount_series-201701241651-csrftests-analyzed.db" \
"oxid-create_product-201701241652-csrftests-analyzed.db" \
"oxid-create_shipping_rule-201701241653-csrftests-analyzed.db" \
"oxid-create_user-201701241653-csrftests-analyzed.db" \
"oxid-logs_in_and_buys_stuff-201701241703-csrftests-analyzed.db" \
"oxid-logs_in_and_changes_email-201701241702-csrftests-analyzed.db" \
"oxid-logs_in_and_changes_password-201701241701-csrftests-analyzed.db" \
"oxid-run_sql-201701241654-csrftests-analyzed.db" \
"oxid-update_category-201701241655-csrftests-analyzed.db" \
"oxid-update_product-201701241655-csrftests-analyzed.db" \
"oxid-update_shipping_rule-201701241656-csrftests-analyzed.db" \
"prestashop-create_category-201701241706-csrftests-analyzed.db" \
"prestashop-create_customer-201701241707-csrftests-analyzed.db" \
"prestashop-create_discount-201701241708-csrftests-analyzed.db" \
"prestashop-create_product-201701241710-csrftests-analyzed.db" \
"prestashop-delete_carrier-201701241710-csrftests-analyzed.db" \
"prestashop-disable_currencies-201701241711-csrftests-analyzed.db" \
"prestashop-disable_module-201701241713-csrftests-analyzed.db" \
"prestashop-disable_some_prefs-201701241714-csrftests-analyzed.db" \
"prestashop-erase_logs-201701241715-csrftests-analyzed.db" \
"prestashop-log_in_buy_stuff-201701241720-csrftests-analyzed.db" \
"prestashop-log_in_change_email-201701241717-csrftests-analyzed.db" \
"prestashop-log_in_create_new_address-201701242216-csrftests-analyzed.db" \
"prestashop-toggle_demomode-201701241717-csrftests-analyzed.db" \
"simpleinvoice-login_and_add_second_admin-201701241728-csrftests-analyzed.db" \
"simpleinvoice-login_and_change_default_tax-201701241725-csrftests-analyzed.db" \
"simpleinvoice-login_and_change_tax_rate-201701241726-csrftests-analyzed.db" \
"simpleinvoice-login_and_enable_paypal-201701241727-csrftests-analyzed.db" \
"simpleinvoice-login_and_new_customer-201701241728-csrftests-analyzed.db" \
"simpleinvoice-login_and_new_invoice-201701241725-csrftests-analyzed.db" \
"simplemachinesforum-change_some_sec_prefs-201701241832-csrftests-analyzed.db" \
"simplemachinesforum-create_new_board-201701241824-csrftests-analyzed.db" \
"simplemachinesforum-create_topic-201701241801-csrftests-analyzed.db" \
"simplemachinesforum-mark_as_read-201701241807-csrftests-analyzed.db" \
"simplemachinesforum-modify_post-201701241818-csrftests-analyzed.db" \
"simplemachinesforum-reply-201701241812-csrftests-analyzed.db" \
"simplemachinesforum-upgrade_account-201701241837-csrftests-analyzed.db" \
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
