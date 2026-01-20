(setup (:package editorconfig)

  (add-hook 'emacs-startup-hook #'editorconfig-mode)

  (:when-loaded

    (when (boundp 'editorconfig-get-properties-function)

      (setopt editorconfig-get-properties-function #'editorconfig-get-properties)
      ;; Only available in https://github.com/editorconfig/editorconfig-emacs

      )

    (setopt editorconfig-trim-whitespaces-mode
            (cond ((package-installed-p 'ws-butler)
                   #'ws-butler-mode)
                  (t nil)))

    (setopt editorconfig-lisp-use-default-indent nil)

    )

  )
