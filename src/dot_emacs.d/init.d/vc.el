;;; VC Mode

(req-package vc
  :init
  ;; Disable VC Mode
  (setq vc-handled-backends '())
  ;; Don't ask if visiting a symlink to a file under version control.
  ;; Follow it.
  (setq vc-follow-symlinks t))
