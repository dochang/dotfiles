(req-package git-annex
  ;; Avoid key binding conflicts.
  :bind-keymap (:map $extended-map
                ("@" . git-annex-dired-map)))
