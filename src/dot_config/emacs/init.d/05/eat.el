(setup (:package eat)

  (:when-loaded

    (setopt eat-line-input-history-isearch 'dwim)

    (setopt eat-eshell-fallback-if-stty-not-available t)

    (setopt eat-enable-mouse t)

    (setopt eat-enable-kill-from-terminal t)

    (setopt eat-enable-yank-to-terminal t)

    (setopt eat-enable-directory-tracking t)

    (setopt eat-enable-shell-prompt-annotation t)

    (setopt eat-enable-auto-line-mode nil)

    (setopt eat-query-before-killing-running-terminal 'auto)

    )

  )
