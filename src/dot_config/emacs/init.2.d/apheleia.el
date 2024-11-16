(setup (:package apheleia)

  (add-hook 'emacs-startup-hook 'apheleia-global-mode)

  (:when-loaded

    (setf (alist-get 'zigfmt apheleia-formatters)
          '("zig" "fmt" "--stdin")
          (alist-get 'taplofmt apheleia-formatters)
          '("taplo" "fmt" "-")
          (alist-get 'sql-formatter apheleia-formatters)
          '("sql-formatter")
          (alist-get 'kdlfmt apheleia-formatters)
          '("kdlfmt" "format" "-")
          (alist-get 'shfmt apheleia-formatters)
          '("shfmt" "--language-dialect" "auto"
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
            "-"))

    (setf (alist-get 'zig-mode apheleia-mode-alist)
          'zigfmt
          (alist-get 'toml-ts-mode apheleia-mode-alist)
          'taplofmt
          (alist-get 'toml-mode apheleia-mode-alist)
          'taplofmt
          (alist-get 'python-ts-mode apheleia-mode-alist)
          '(black isort)
          (alist-get 'python-mode apheleia-mode-alist)
          '(black isort)
          (alist-get 'sql-mode apheleia-mode-alist)
          'sql-formatter
          (alist-get 'kdl-mode apheleia-mode-alist)
          'kdlfmt
          (alist-get 'kdl-ts-mode apheleia-mode-alist)
          'kdlfmt
          (alist-get 'bats-mode apheleia-mode-alist)
          'shfmt)

    )

  )
