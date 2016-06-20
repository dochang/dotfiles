;;; JS Mode

(defun $js-mode-hook ()
  ($prog-mode-hook*)
  ($camel-case-mode 1))

(req-package js
  :loader :built-in
  :init
  (setq js-indent-level 2)
  (add-hook 'js-mode-hook '$js-mode-hook))
