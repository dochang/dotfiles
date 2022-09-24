(req-package idle-org-agenda
  :hook (emacs-startup . idle-org-agenda-mode)
  :init
  (setq idle-org-agenda-interval (* 60 30)))
