(setup git-commit
  (:package magit)
  ;; `git-commit' has been merged into `magit'.  It must not be installed, in
  ;; order to load the proper `git-commit' library from `magit'.
  ;;
  ;; https://github.com/magit/magit/commit/9be8a4ab7a7dc6f34615f1011e8da263651c8f87

  (:when-loaded

    (define-key git-commit-redundant-bindings
                "\C-\M-i" 'completion-at-point)
    ;; For conventional-commit

    )

  )
