(defun $dns-mode-hook ()
  ($run-prog-mode-hook)
  (setq-local tab-always-indent nil))

(req-package dns-mode
  :hook (dns-mode . $dns-mode-hook))
