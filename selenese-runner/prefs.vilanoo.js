// No cache
user_pref("browser.cache.disk.enable", false);

// No prefetching
user_pref("network.prefetch-next", false);

// Kill switch for Telemetry
user_pref("datareporting.policy.dataSubmissionEnabled", false);

// Remove br data compression support (vilanoo does not support it)
user_pref("network.http.accept-encoding.secure", "gzip,deflate");

// Certificate validation
user_pref("security.OCSP.enabled", 0);

// No auto updates
user_pref("app.update.auto", false);
user_pref("app.update.enabled", false);
user_pref("browser.search.update", false);
user_pref("extensions.update.autoUpdate", false);
user_pref("extensions.update.autoUpdateEnabled", false);
user_pref("extensions.update.enabled", false)

// No safe browsing
user_pref("browser.safebrowsing.downloads.remote.enabled", false);
user_pref("browser.safebrowsing.enabled", false);
user_pref("browser.safebrowsing.malware.enabled", false);

// List of domains which are not proxies (this should help vilanoo)
user_pref("network.proxy.type", 1);
user_pref("network.proxy.no_proxies_on", "*.com, *.net, *.org");
user_pref("network.proxy.ftp", "127.0.0.1");
user_pref("network.proxy.ftp_port", 8080);
user_pref("network.proxy.http", "127.0.0.1");
user_pref("network.proxy.http_port", 8080);
user_pref("network.proxy.share_proxy_settings", true);
user_pref("network.proxy.socks", "127.0.0.1");
user_pref("network.proxy.socks_port", 8080);
user_pref("network.proxy.ssl", "127.0.0.1");
user_pref("network.proxy.ssl_port", 8080);

