(when (< emacs-major-version 29)
  ;; Use elisp-autofmt instead for Emacs 29+.

  (setup elfmt

    (quelpa '(elfmt :repo "riscy/elfmt" :fetcher github) :upgrade nil)
    ;; Install `elfmt' by `quelpa' rather than `package-vc-install', because
    ;; there is no `package-vc-install' before Emacs 29.

    ;; (:with-mode (emacs-lisp-mode)
    ;;   (:hook elfmt-mode))
    ;; It doesn't work as I expected.

    )

  )
