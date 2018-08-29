(req-package lsp-sh
  :ensure nil
  :quelpa (lsp-sh :fetcher github :repo "emacs-lsp/lsp-sh")
  :hook (sh-mode . lsp-sh-enable))
