(setup (:package apheleia)

  (add-hook 'emacs-startup-hook 'apheleia-global-mode)

  (:when-loaded

    (setf (alist-get 'zigfmt apheleia-formatters)
          '("zig" "fmt" "--stdin")
          (alist-get 'taplofmt apheleia-formatters)
          '("taplo" "fmt" "-")
          (alist-get 'sql-formatter apheleia-formatters)
          '("sql-formatter"))

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
          'sql-formatter)

    )

  )
