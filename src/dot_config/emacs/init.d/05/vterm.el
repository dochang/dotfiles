(setup (:package vterm)

  (:when-loaded

    (setopt vterm-always-compile-module t)

    (setopt vterm-kill-buffer-on-exit t)
    ;; t is better.  If you want to restart vterm, you have to kill the buffer
    ;; first.  Why not kill the buffer on exit?

    (keymap-set vterm-mode-map "C-q" 'vterm-send-next-key)

    (keymap-set vterm-copy-mode-map "C-w" 'vterm-copy-mode-done)

    )

  )
