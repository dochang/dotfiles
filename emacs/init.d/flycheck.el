(req-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (flycheck-yamllint-setup)
  (flycheck-package-setup)
  (flycheck-gometalinter-setup))
