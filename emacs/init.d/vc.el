;;; VC Mode

(req-package vc
  :custom
  ;; Disable VC Mode
  (vc-handled-backends '())
  ;; Don't ask if visiting a symlink to a file under version control.
  ;; Follow it.
  (vc-follow-symlinks t))
