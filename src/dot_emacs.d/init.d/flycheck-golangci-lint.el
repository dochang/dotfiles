(req-package flycheck-golangci-lint
  :after flycheck
  :init
  (flycheck-golangci-lint-setup))
