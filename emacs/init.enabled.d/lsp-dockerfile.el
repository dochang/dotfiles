(req-package lsp-dockerfile
  :ensure nil
  :quelpa (lsp-dockerfile :fetcher github :repo "emacs-lsp/lsp-dockerfile")
  :hook (dockerfile-mode . lsp-dockerfile-enable))
