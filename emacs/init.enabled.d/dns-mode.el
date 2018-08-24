(defun $dns-mode-hook ()
  ($run-prog-mode-hook)
  (setq indent-tabs-mode t)
  (setq-local tab-always-indent nil))

(req-package dns-mode
  :init
  (add-hook 'dns-mode-hook '$dns-mode-hook))
