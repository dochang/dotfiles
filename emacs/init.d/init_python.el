(defun $python-mode-hook ()
  ($prog-mode-hook*)
  ;; aggressive-indent-mode will break the indentation.  Disable it.
  (aggressive-indent-mode -1))

(req-package python
  :loader :built-in
  :init
  (add-hook 'python-mode-hook '$python-mode-hook))
