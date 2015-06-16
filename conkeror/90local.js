// -*- mode: js; indent-tabs-mode: nil; js-indent-level: 4; -*-


//// Proxy Manager
function proxy_reload_pac () {
    if (get_pref("network.proxy.autoconfig_url")) {
        Components.classes["@mozilla.org/network/protocol-proxy-service;1"].getService().reloadPAC();
    }
}

interactive("proxy-reload-pac", "Reload PAC", proxy_reload_pac);

interactive("proxy-manager", "Proxy Manager", function (I) {
    var pac_url = get_pref("network.proxy.autoconfig_url");
    if (pac_url) {
        var pac_file = make_file(pac_url.slice(7)); // remove "file://"
        yield open_file_with_external_editor(pac_file);
        proxy_reload_pac();
    }
});

(function () {
    var pac_url = getenv("auto_proxy");
    if (pac_url) {
        session_pref("network.proxy.autoconfig_url", pac_url);
        session_pref("network.proxy.type", 2);
    }
})();


//// Try to fix freezing
/// https://wiki.mozilla.org/User_talk:Comrade693/SQLite_3.6.4_Security_Review#Configuration
/// http://code.google.com/p/dactyl/issues/detail?id=156
/// http://code.google.com/p/dactyl/issues/detail?id=692
/// http://nicklothian.com/blog/2009/05/05/fixing-firefox-performance-and-lock-ups-on-linux/
session_pref("toolkit.storage.synchronous", 0);


function multi_follow (I) {
    I.target = OPEN_NEW_BUFFER_BACKGROUND;
    var element = yield read_browser_object(I);
    while (element) {
        try {
            element = load_spec(element);
            if (I.forced_charset) {
                element.forced_charset = I.forced_charset;
            }
        } catch (e) {}
        browser_object_follow(I.buffer, I.target, element);
        element = yield read_browser_object(I);
    }
}

interactive("multi-follow", null, multi_follow, $browser_object = browser_object_links);
