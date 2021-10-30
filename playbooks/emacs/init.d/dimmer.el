(req-package dimmer

  :hook (emacs-startup . dimmer-mode)

  :config

  ;; (dimmer-configure-company-box)
  ;; It doesn't work.  Don't the reason for now.
  (dimmer-configure-helm)
  (dimmer-configure-gnus)
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-posframe)
  (dimmer-configure-which-key)

  )
