(when (>= emacs-major-version 29)
  ;; Use elfmt instead for Emacs < 29.

  (setup (:package elisp-autofmt)

    (:with-mode (emacs-lisp-mode)
      (:hook elisp-autofmt-mode))

    (:when-loaded

      (:option elisp-autofmt-style 'native)
      (:option elisp-autofmt-format-quoted t)

      )

    )

  )
