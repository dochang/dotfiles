(req-package lsp-dockerfile
  :ensure (lsp-dockerfile :pin :quelpa)
  :quelpa (lsp-dockerfile :fetcher github :repo "emacs-lsp/lsp-dockerfile")
  :hook (dockerfile-mode . lsp-dockerfile-enable))
