(defun $rjsx-mode-hook ()
  ($run-prog-mode-hook)
  ($camel-case-mode 1))

(req-package rjsx-mode
  :mode "\\.jsx?\\'"
  :hook (rjsx-mode . $rjsx-mode-hook))
