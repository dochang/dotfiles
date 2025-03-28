(setup (:package aggressive-indent)

  ;; (add-hook 'emacs-startup-hook 'global-aggressive-indent-mode)
  (:with-mode (lisp-data-mode scheme-mode clojure-mode clojure-ts-mode)
    (:hook aggressive-indent-mode))
  ;; aggressive-indent-mode is unneeded since format-all and emacs-lsp are
  ;; released.  Enable it on demand for the modes which don't support
  ;; format-all and LSP formatting.

  (:when-loaded

    (setopt aggressive-indent-dont-electric-modes t)
    ;; `electric-indent-mode' should be disabled.  Otherwise the variable
    ;; `electric-indent-mode' is t even `aggressive-indent-mode' is enabled.
    ;;
    ;; The variable `electric-indent-mode' controls
    ;; `electric-newline-and-maybe-indent' (`C-j').  If `electric-indent-mode'
    ;; is t, `electric-newline-and-maybe-indent' just inserts a newline, no
    ;; indenting.

    (setopt aggressive-indent-excluded-modes
            (seq-uniq
             (append
              '(special-mode
                dockerfile-mode
                makefile-mode
                python-base-mode
                yaml-ts-mode
                yaml-mode)
              aggressive-indent-excluded-modes)))
    ;; aggressive-indent-mode will break the indentation in these modes.

    )

  )
