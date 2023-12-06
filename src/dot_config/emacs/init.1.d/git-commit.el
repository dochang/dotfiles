(setup (:package git-commit)

  (:when-loaded

    (define-key git-commit-redundant-bindings
                "\C-\M-i" 'completion-at-point)
    ;; For conventional-commit

    )

  )
