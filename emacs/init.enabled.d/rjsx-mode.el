(defun $rjsx-mode-hook ()
  ($prog-mode-hook*)
  ($camel-case-mode 1))

(req-package rjsx-mode
  :mode "\\.jsx?\\'"
  :init
  (add-hook 'rjsx-mode-hook '$rjsx-mode-hook))
