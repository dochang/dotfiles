(setup (:package editorconfig)

  (add-hook 'emacs-startup-hook 'editorconfig-mode)

  (:when-loaded

    (when (boundp 'editorconfig-get-properties-function)

      (setopt editorconfig-get-properties-function 'editorconfig-get-properties)
      ;; Only available in https://github.com/editorconfig/editorconfig-emacs

      )

    (setopt editorconfig-trim-whitespaces-mode
            (cond ((package-installed-p 'ws-butler)
                   'ws-butler-mode)
                  (t nil)))

    (setopt editorconfig-lisp-use-default-indent nil)

    (setopt editorconfig-indentation-alist
            (seq-reduce
             (lambda (alist elem)
               (if (seq-contains-p alist elem)
                   alist
                 (cons elem alist)))
             '(
               (kdl-mode tab-width)
               (kdl-ts-mode kdl-ts-mode-indent-offset)
               (mermaid-mode mermaid-indentation-level)
               (mermaid-ts-mode mermaid-ts-indent-level)
               (LilyPond-mode LilyPond-indent-level)
               (sxhkd-mode sxhkd-mode-indentation-length)
               (json5-ts-mode json5-ts-mode-indent-offset)
               )
             editorconfig-indentation-alist))

    )

  )
