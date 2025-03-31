(setup magit-mode
  (:package magit)

  (:when-loaded

    ;; Don't pop a new window.
    (:option magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

    )

  )
