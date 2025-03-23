(setup emacs

  (setq tab-always-indent 'complete)

  (keymap-set indent-rigidly-map "<backtab>" 'indent-rigidly-left)
  (keymap-set indent-rigidly-map "<" 'indent-rigidly-left)
  (keymap-set indent-rigidly-map ">" 'indent-rigidly-right)
  (keymap-set indent-rigidly-map "M-<" 'indent-rigidly-left-to-tab-stop)
  (keymap-set indent-rigidly-map "M->" 'indent-rigidly-right-to-tab-stop)

  )
