;;; Makefile Mode

(defun $makefile-mode-hook ()
  (setq indent-tabs-mode t))

(req-package make-mode
  :hook (makefile-mode . $makefile-mode-hook))
