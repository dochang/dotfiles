(setup (:package aggressive-indent)

  ;; (add-hook 'emacs-startup-hook 'global-aggressive-indent-mode)
  (:with-mode (lisp-data-mode scheme-mode clojure-mode clojure-ts-mode)
    (:hook aggressive-indent-mode))
  ;; aggressive-indent-mode is unneeded since many languages already have
  ;; their own formatters.  Enable it on demand for the modes which don't have
  ;; a formatter.
  ;;
  ;; Also, never enable aggressive-indent-mode for all languages which use
  ;; whitespace indentations (aka off-side rule).
  ;;
  ;; https://en.wikipedia.org/wiki/Off-side_rule

  (:when-loaded

    (setopt aggressive-indent-dont-electric-modes t)
    ;; `electric-indent-mode' should be disabled.  Otherwise the variable
    ;; `electric-indent-mode' is t even `aggressive-indent-mode' is enabled.
    ;;
    ;; The variable `electric-indent-mode' controls
    ;; `electric-newline-and-maybe-indent' (`C-j').  If `electric-indent-mode'
    ;; is t, `electric-newline-and-maybe-indent' just inserts a newline, no
    ;; indenting.

    )

  )
