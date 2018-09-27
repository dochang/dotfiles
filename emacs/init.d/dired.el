;;; Dired Mode

;; Adding functions to `dired-load-hook' has no effect.  `dired' is an essential
;; package which may be loaded during the startup.

(req-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lhA")
  (dired-dwim-target t))
