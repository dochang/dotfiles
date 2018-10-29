(req-package lsp-ruby
  :ensure (lsp-ruby :pin :quelpa)
  :quelpa (lsp-ruby :fetcher github :repo "emacs-lsp/lsp-ruby")
  :hook (ruby-mode . lsp-ruby-enable))
