;;; Makefile Mode

(defun $makefile-mode-hook ()
  ($prog-mode-hook*)
  (setq indent-tabs-mode t))

(req-package make-mode
  :loader :built-in
  :init
  (add-hook 'makefile-mode-hook '$makefile-mode-hook))
