(setup conventional-commit

  (unless (package-installed-p 'conventional-commit)
    (package-vc-install "https://github.com/akirak/conventional-commit.el"))

  (:with-mode (git-commit-mode)
    (:hook conventional-commit-setup))

  (:when-loaded

    (setq conventional-commit-type-list
          (remove "chore" conventional-commit-type-list))
    ;; https://github.com/commitizen-tools/commitizen/issues/142#issuecomment-596251632
    ;; https://github.com/angular/angular/blob/main/CONTRIBUTING.md#type
    ;; https://nitayneeman.com/posts/understanding-semantic-commit-messages-using-git-and-angular/#build
    ;; https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#type

    )

  )
