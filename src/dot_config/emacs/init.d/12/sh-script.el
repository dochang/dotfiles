(with-eval-after-load 'apheleia

  (setopt apheleia-formatters
          (cons '(shfmt "shfmt" "--language-dialect" "auto"
                        (when buffer-file-name
                          (list "--filename" buffer-file-name))
                        ;; Use `buffer-file-name' instead of `(quote filepath)'.  A Form
                        ;; is only allowed to return either a string or a list of strings.
                        (when apheleia-formatters-respect-indent-level
                          (list "--indent"
                                (number-to-string
                                 (cond
                                  (indent-tabs-mode 0)
                                  ((boundp 'sh-basic-offset)
                                   sh-basic-offset)
                                  (t 0)))))
                        "-")
                apheleia-formatters))

  )

(defun $sh-base-mode-hook ()
  (smartparens-strict-mode -1))

(setup (:package sh-script)

  (add-hook 'sh-base-mode-hook '$sh-base-mode-hook)

  (:when-loaded

    (with-eval-after-load 'apheleia

      (setopt apheleia-mode-alist
              (cons '(sh-mode . shfmt)
                    apheleia-mode-alist))

      )

    )

  )
