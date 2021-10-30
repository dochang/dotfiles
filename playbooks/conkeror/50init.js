// -*- mode: js; indent-tabs-mode: nil; js-indent-level: 4; -*-

//// Mix in non-conflict functions to Underscore namespace
_s = _.string;
_.mixin(_s.exports());


//// Basic bindings
_({
    "M-left": "back",
    "M-right": "forward",
    "scroll_lock": "caret-mode"
}).each(function (cmd, seq) {
    define_key(content_buffer_normal_keymap, seq, cmd);
});


//// Define vi-like bindings
_([content_buffer_normal_keymap, help_buffer_keymap]).each(function (kmap) {
    _({
        'k': 'cmd_scrollLineUp',
        'j': 'cmd_scrollLineDown',
        'h': 'cmd_scrollLeft',
        'l': 'cmd_scrollRight'
    }).each(function (cmd, seq) {
        define_key(kmap, seq, cmd);
    });
});


//// User Variables
/// http://conkeror.org/UserVariables

/// Don't kill the last buffer
can_kill_last_buffer = false;

/// Display the URL in a panel above the minibuffer when selecting a
/// hint
hints_display_url_panel = true;


//// Confirm when quit
/// http://conkeror.org/Tips#Askbeforeclosingthewindow
add_hook("before_quit_hook", function () {
    var w = get_recent_conkeror_window();
    var result = (w == null) || "y" == (yield w.minibuffer.read_single_character_option(
        $prompt = "Quit Conkeror? (y/n)",
        $options = ["y", "n"]
    ));
    yield co_return(result);
});


//// Download Buffer
/// http://conkeror.org/Downloads

/// Open download buffer in a new buffer in the current window
download_buffer_automatic_open_target = OPEN_NEW_BUFFER_BACKGROUND;
/// Don't open download buffer automatically
remove_hook("download_added_hook", open_download_buffer_automatically);


//// Content Handlers
/// http://conkeror.org/ContentHandlers
external_content_handlers.set("image/*", "sxiv -f");


//// Load url in new buffer
/// http://conkeror.org/FAQ#HowdoImakelinksopenedfromotherapplicationsopeninanexistingConkerorwindow.3F
url_remoting_fn = load_url_in_new_buffer;


//// Disable formfill
/// http://conkeror.org/BreakingChanges#September6.2C2011
session_pref("browser.formfill.enable", false);


//// Webjumps
/// http://conkeror.org/Webjumps
/// http://conkeror.org/WritingWebjumps

/// EmacsWiki
// http://conkeror.org/Webjumps#Emacs
define_webjump(
    "emacswiki",
    "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&q=%s&sa=Search&siteurl=emacswiki.org%2F",
    $alternative = "http://emacswiki.org/"
);

/// Worg
// http://conkeror.org/Webjumps#Org-Mode
// http://orgmode.org/worg/org-hacks.html#sec-3-5
define_webjump(
    "worg",
    "http://www.google.com/cse?cx=002987994228320350715%3Az4glpcrritm&q=%s&sa=Search&siteurl=orgmode.org%2Fworg%2F",
    $alternative = "http://orgmode.org/worg/"
);

/// Wikipedia
require('page-modes/wikipedia');
_(['en', 'zh', 'ja']).each(function (i) {
    var rest = wikipedia_versions[i];
    var name = string_format(wikipedia_webjumps_format, {s: i});
    define_webjump(name, "https://" + i + ".wikipedia.org/wiki/" + rest.search);
});
