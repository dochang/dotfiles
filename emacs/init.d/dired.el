;;; Dired Mode

;; Adding functions to `dired-load-hook' has no effect.  `dired' is an essential
;; package which may be loaded during the startup.

(req-package dired
  :ensure nil
  :init
  (setq dired-listing-switches "-lhA")
  (setq dired-dwim-target t))
