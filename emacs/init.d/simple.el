(req-package simple
  :ensure nil

  :hook (emacs-startup . column-number-mode)

  :init

  ;; Don't delete trailing lines when calling `delete-trailing-whitespace' on
  ;; the entire buffer.
  (setq delete-trailing-lines nil)

  :config

  ;; Special Mode

  (unless (lookup-key special-mode-map "z")
    (define-key special-mode-map "z" 'kill-this-buffer))
  ;; Emacs has deleted `z' binding in GIT#0d4505d & GIT#82dffff .  We
  ;; restore it here.

  )
