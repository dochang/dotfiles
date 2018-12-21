(req-package lsp-clients
  :ensure lsp-mode
  :after lsp
  :custom
  (lsp-clients-go-server "bingo")
  (lsp-clients-go-executable-path (executable-find "bingo"))
  (lsp-clients-go-language-server-flags '())
  :init
  (require 'lsp-clients))
