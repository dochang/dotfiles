(req-package lsp-ruby
  :ensure nil
  :quelpa (lsp-ruby :fetcher github :repo "emacs-lsp/lsp-ruby")
  :hook (ruby-mode . lsp-ruby-enable))
