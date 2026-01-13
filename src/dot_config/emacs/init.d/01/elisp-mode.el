(setup elisp-mode

  (:file-match "Cask\\'")

  (:when-loaded

    (keymap-unset lisp-interaction-mode-map "C-j" t)
    ;; Emacs binds this key to `eval-print-last-sexp'.  I don't like this
    ;; binding.  I always print result by `C-u C-x C-e'.  Restore its binding
    ;; to `electric-newline-and-maybe-indent'.
    ;;
    ;; As `bind-key' binds the key to `(or lisp-interaction-mode-map
    ;; global-map)', the global binding may be overridden.  If it happens,
    ;; bind the key to `electric-newline-and-maybe-indent' explicitly.

    ))
