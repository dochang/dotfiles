(defun $js2-mode-hook ()
  ($prog-mode-hook*)
  ($camel-case-mode 1))

(req-package js2-mode
  :init
  (setq js2-strict-trailing-comma-warning nil)
  (add-hook 'js2-mode-hook '$js2-mode-hook))
