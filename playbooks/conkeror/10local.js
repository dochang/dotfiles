// -*- mode: js; indent-tabs-mode: nil; js-indent-level: 4; -*-

// clear the startup-cache so that modules and the user's rc are
// loaded from disk, not from a cache.  this problem is a
// characteristic of using mozIJSSubScriptLoader.loadSubScript as our
// primary means of loading, since XULRunner 8.0.
//
// See commit ce6147f, 937e4f7, f206b95, 5d5d1bc in conkeror's repo.
(function () {
    var obs = Cc["@mozilla.org/observer-service;1"]
        .getService(Ci.nsIObserverService);
    obs.notifyObservers(null, "startupcache-invalidate", null);
})();


//// Display scrollbar in the first buffer of window
//// Only for conkeror < 1.0pre && xulrunner >= 2
/// http://conkeror.org/UpstreamBugs#missingscrollbarinfirstbufferofwindow.28xulrunner.3E.3D2.29
add_hook("create_buffer_late_hook", function (buffer) {
    buffer.top_frame.scrollbars.visible = true;
});
