(req-package lsp-javascript-flow
  :hook ((js-mode js2-mode rjsx-mode) . lsp-javascript-flow-enable))
