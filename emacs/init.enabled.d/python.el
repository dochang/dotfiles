(defun $python-mode-hook ()
  ($run-prog-mode-hook)
  ;; aggressive-indent-mode will break the indentation.  Disable it.
  (aggressive-indent-mode -1))

(req-package python
  :hook (python-mode . $python-mode-hook))
