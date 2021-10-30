finder.setupFindbar();
let findbar = finder.findbar;

// https://github.com/vimperator/vimperator-labs/issues/676
if ((!("_setHighlightTimeout" in findbar)) && ("toggleHighlight" in findbar)) {
    findbar._setHighlightTimeout = (() => findbar.toggleHighlight(true));
}
