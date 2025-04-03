(setup (:package highlight-indent-guides)

  (:with-mode (prog-mode)
    (:hook highlight-indent-guides-mode))

  (:when-loaded

    (setopt highlight-indent-guides-suppress-auto-error t)
    ;; https://github.com/DarthFennec/highlight-indent-guides/issues/83#issuecomment-635621246

    )

  )
