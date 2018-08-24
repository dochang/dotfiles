(req-package aggressive-indent
  :init
  (setq aggressive-indent-dont-electric-modes t)
  ;; `electric-indent-mode' should be disabled.  Otherwise the variable
  ;; `electric-indent-mode' is t even `aggressive-indent-mode' is enabled.
  ;;
  ;; The variable `electric-indent-mode' controls
  ;; `electric-newline-and-maybe-indent' (`C-j').  If `electric-indent-mode' is
  ;; t, `electric-newline-and-maybe-indent' just inserts a newline, no
  ;; indenting.
  (add-to-list 'aggressive-indent-excluded-modes 'dockerfile-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'makefile-mode)
  ;; aggressive-indent-mode will break the indentation in these modes.
  )
