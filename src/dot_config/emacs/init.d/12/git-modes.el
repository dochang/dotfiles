;; `git-modes' are major modes for git files although it's a magit package.

(setup (:package git-modes)

  (with-eval-after-load 'gitattributes-mode

    (setopt gitattributes-mode-enable-eldoc t)

    )

  )
