(req-package git-annex
  :config
  ;; Avoid key binding conflicts.
  (define-key $extended-map "@" git-annex-dired-map))
