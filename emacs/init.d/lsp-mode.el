(define-advice lsp--suggest-project-root (:after-until (&optional args) fallback-to-default-directory)
  default-directory)

(defun $lsp-install-before-save-hooks ()
  (add-hook 'before-save-hook 'lsp-format-buffer nil 'local)
  (add-hook 'before-save-hook 'lsp-organize-imports nil 'local))

(req-package lsp-mode
  :ensure lsp-mode
  :hook ((prog-mode . lsp-deferred)
         (prog-mode . $lsp-install-before-save-hooks))
  :custom
  (lsp-auto-guess-root t)
  ;; https://github.com/emacs-lsp/lsp-python/issues/28#issuecomment-437599058
  ;; https://github.com/emacs-lsp/lsp-mode/pull/470#issuecomment-437600636
  (lsp-auto-configure t)
  (lsp-enable-snippet nil)
  )
