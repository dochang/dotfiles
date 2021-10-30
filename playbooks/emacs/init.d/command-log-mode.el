(req-package command-log-mode

  :commands (global-command-log-mode)

  :init

  (setq command-log-mode-auto-show nil)
  (setq command-log-mode-key-binding-open-log nil)
  (setq command-log-mode-is-global nil)
  (setq command-log-mode-open-log-turns-on-mode t)

  )
