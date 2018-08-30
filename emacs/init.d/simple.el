(req-package simple
  :ensure nil

  :hook (emacs-startup . column-number-mode)

  :init

  ;; `compose-mail' will warn if `mail-self-blind' &
  ;; `compose-mail-user-agent-warnings' are both set to `t'.
  ;;
  ;; Suppress this warning.
  (setq compose-mail-user-agent-warnings nil)

  ;; Use `message-user-agent' for mail composition.
  ;; [[info:emacs#Mail%20Methods]]
  (setq mail-user-agent 'message-user-agent)

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
