(req-package flycheck-gometalinter
  :init
  ;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
  (setq flycheck-gometalinter-vendor t)
  ;; Reset enabled linters.  See [1] for details.
  ;;
  ;; [1]: https://raw.githubusercontent.com/dasfoo/travis/master/go.sh
  (setq flycheck-gometalinter-disable-linters
        '("gas"))
  (setq flycheck-gometalinter-enable-linters
        '("testify" "test" "goimports" "gofmt" "lll" "misspell" "unused"))
  ;; Some linters are too slow.  Disable them.
  (setq flycheck-gometalinter-fast t))
