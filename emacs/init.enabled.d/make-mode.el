;;; Makefile Mode

(defun $makefile-mode-hook ()
  ($run-prog-mode-hook)
  ;; aggressive-indent-mode will break the indentation.  Disable it.
  (aggressive-indent-mode -1)
  (setq indent-tabs-mode t))

(req-package make-mode
  :init
  (add-hook 'makefile-mode-hook '$makefile-mode-hook))
