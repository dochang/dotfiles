(defun $python-mode-hook ()
  (setq format-all-formatters '(("Python" isort black))))

(req-package python
  :hook (python-mode . $python-mode-hook))
