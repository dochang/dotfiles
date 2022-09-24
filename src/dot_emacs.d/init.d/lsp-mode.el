(define-advice lsp--suggest-project-root (:after-until (&optional args) fallback-to-default-directory)
  default-directory)

(defun $lsp-install-before-save-hooks ()
  (add-hook 'before-save-hook 'lsp-format-buffer nil 'local)
  (add-hook 'before-save-hook 'lsp-organize-imports nil 'local))

(req-package lsp-mode
  :ensure lsp-mode
  :hook ((prog-mode . lsp-deferred)
         (prog-mode . $lsp-install-before-save-hooks))
  :init
  (setq lsp-auto-guess-root t)
  ;; https://github.com/emacs-lsp/lsp-python/issues/28#issuecomment-437599058
  ;; https://github.com/emacs-lsp/lsp-mode/pull/470#issuecomment-437600636
  (setq lsp-auto-configure t)
  (setq lsp-enable-snippet nil)
  ;; (setq lsp-idle-delay 2)
  ;; Default value (0.200 sec) is too fast.  Some major modes will call
  ;; formatter on `save-buffer'.  The formatter reformat my code entirely.
  ;; Since `lsp-on-change' saves the buffer, it will call `save-buffer' every
  ;; time I type in the buffer, even if the typing isn't completed.  This
  ;; breaks my code.  So, set `lsp-idle-delay' to a bigger value.
  (setq lsp-keymap-prefix "C-c /")
  )
