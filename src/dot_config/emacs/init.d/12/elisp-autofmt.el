(when (>= emacs-major-version 29)
  ;; Use elfmt instead for Emacs < 29.

  (setup (:package elisp-autofmt)

    (:with-mode (emacs-lisp-mode)
      (:hook elisp-autofmt-mode))

    (:when-loaded

      (setopt elisp-autofmt-style 'native)
      (setopt elisp-autofmt-format-quoted t)

      )

    )

  )
