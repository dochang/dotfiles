(req-package flycheck-gometalinter
  :init
  ;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
  (setq flycheck-gometalinter-vendor t))
