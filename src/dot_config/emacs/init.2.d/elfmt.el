(when (< emacs-major-version 29)
  ;; Use elisp-autofmt instead for Emacs 29+.

  (setup elfmt

    (quelpa '(elfmt :repo "riscy/elfmt" :fetcher github) :upgrade nil)

    ;; (:with-mode (emacs-lisp-mode)
    ;;   (:hook elfmt-mode))
    ;; It doesn't work as I expected.

    )

  )
