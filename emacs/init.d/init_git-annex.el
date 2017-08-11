(req-package git-annex
  ;; Avoid key binding conflicts.
  ;;
  ;; `:bind-keymap' does not support `:map'.  Use `:init' instead.
  :init
  (define-key $extended-map "@" 'git-annex-dired-map))
