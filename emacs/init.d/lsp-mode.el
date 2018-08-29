(req-package lsp-mode
  :hook (lsp-after-open . lsp-enable-imenu)
  :init
  (autoload 'lsp-enable-imenu "lsp-imenu" nil t))
