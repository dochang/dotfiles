(req-package aggressive-indent
  :hook (emacs-startup . global-aggressive-indent-mode)
  :init
  (setq aggressive-indent-dont-electric-modes t)
  ;; `electric-indent-mode' should be disabled.  Otherwise the variable
  ;; `electric-indent-mode' is t even `aggressive-indent-mode' is enabled.
  ;;
  ;; The variable `electric-indent-mode' controls
  ;; `electric-newline-and-maybe-indent' (`C-j').  If `electric-indent-mode' is
  ;; t, `electric-newline-and-maybe-indent' just inserts a newline, no
  ;; indenting.
  :config
  (setq aggressive-indent-excluded-modes
        (delete-dups
         (append
          '(special-mode
            dockerfile-mode
            makefile-mode
            python-mode
            yaml-mode)
          aggressive-indent-excluded-modes)))
  ;; aggressive-indent-mode will break the indentation in these modes.
  )
