(req-package lsp-mode
  :init
  (autoload 'lsp-enable-imenu "lsp-imenu" nil t)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))
