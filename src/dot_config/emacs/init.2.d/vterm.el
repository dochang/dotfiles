(setup (:package vterm)

  (setq vterm-always-compile-module t)

  (setq vterm-kill-buffer-on-exit t)
  ;; t is better.  If you want to restart vterm, you have to kill the buffer
  ;; first.  Why not kill the buffer on exit?

  (:when-loaded

    (keymap-set vterm-mode-map "C-q" 'vterm-send-next-key)

    (keymap-set vterm-copy-mode-map "M-w" 'vterm-copy-mode-done)

    )

  )
