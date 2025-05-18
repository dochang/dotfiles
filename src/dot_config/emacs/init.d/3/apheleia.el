(setup (:package apheleia)

  (add-hook 'emacs-startup-hook 'apheleia-global-mode)

  (:when-loaded

    (setopt apheleia-formatters
            (seq-reduce
             (lambda (formatters formatter)
               (setq formatters
                     (cons formatter
                           (seq-remove (lambda (x) (eq (car x) (car formatter)))
                                       formatters))))
             '(
               (prettier-jsonc "apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=jsonc"
                               (apheleia-formatters-indent "--use-tabs" "--tab-width" 'js-indent-level))
               (prettier-json5 "apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=json5"
                               (apheleia-formatters-indent "--use-tabs" "--tab-width" 'js-indent-level))
               (taplofmt "taplo" "fmt" "-")
               (sql-formatter "sql-formatter")
               (kdlfmt "kdlfmt" "format" "-")
               (shfmt "shfmt" "--language-dialect" "auto"
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
               (google-java-format "google-java-format" "--aosp" "-")
               (ly "ly"
                   (format "%s indent-tabs=false; indent; reformat"
                           (if apheleia-formatters-respect-indent-level
                               (format "indent-width=%d;" LilyPond-indent-level)
                             "")))
               )
             apheleia-formatters))

    (setopt apheleia-mode-alist
            (seq-reduce
             (lambda (alist elem)
               (setq alist
                     (cons elem
                           (seq-remove (lambda (x) (eq (car x) (car elem)))
                                       alist))))
             '(
               (jsonc-mode . prettier-jsonc)
               (toml-ts-mode . taplofmt)
               (toml-mode . taplofmt)
               (python-ts-mode black isort)
               (python-mode black isort)
               (sql-mode . sql-formatter)
               (kdl-mode . kdlfmt)
               (kdl-ts-mode . kdlfmt)
               (sh-mode . shfmt)
               (bats-mode . shfmt)
               (LilyPond-mode . ly)
               )
             apheleia-mode-alist))

    )

  )
