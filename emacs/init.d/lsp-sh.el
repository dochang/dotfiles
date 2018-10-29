(req-package lsp-sh
  :ensure (lsp-sh :pin :quelpa)
  :quelpa (lsp-sh :fetcher github :repo "emacs-lsp/lsp-sh")
  :hook (sh-mode . lsp-sh-enable))
