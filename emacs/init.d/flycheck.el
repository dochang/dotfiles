(req-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-disabled-checkers '())
  :config
  (flycheck-yamllint-setup)
  (flycheck-package-setup)
  (flycheck-gometalinter-setup))
