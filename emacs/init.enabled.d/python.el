(defun $python-mode-hook ()
  ($run-prog-mode-hook)
  ;; aggressive-indent-mode will break the indentation.  Disable it.
  (aggressive-indent-mode -1))

(req-package python
  :init
  (add-hook 'python-mode-hook '$python-mode-hook))
