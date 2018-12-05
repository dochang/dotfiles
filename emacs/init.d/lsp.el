(define-advice lsp--suggest-project-root (:after-until (&optional args) fallback-to-default-directory)
  default-directory)

(req-package lsp
  :ensure lsp-mode
  :hook (prog-mode . lsp)
  :custom
  (lsp-auto-guess-root t)
  ;; https://github.com/emacs-lsp/lsp-python/issues/28#issuecomment-437599058
  ;; https://github.com/emacs-lsp/lsp-mode/pull/470#issuecomment-437600636
  )
