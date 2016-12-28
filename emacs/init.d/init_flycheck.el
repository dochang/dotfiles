(defun $flycheck-mode-hook ()
  (flycheck-rust-setup))

(req-package flycheck
  :init
  (add-hook 'flycheck-mode-hook '$flycheck-mode-hook)
  :config
  (flycheck-package-setup)
  (flycheck-gometalinter-setup))
