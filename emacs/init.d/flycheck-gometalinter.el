(req-package flycheck-gometalinter
  :custom
  ;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
  (flycheck-gometalinter-vendor t)
  ;; Reset enabled linters.  See [1] for details.
  ;;
  ;; [1]: https://raw.githubusercontent.com/dasfoo/travis/master/go.sh
  (flycheck-gometalinter-disable-linters
   '("gas"))
  (flycheck-gometalinter-enable-linters
   '("testify" "test" "goimports" "gofmt" "lll" "misspell" "unused"))
  ;; Some linters are too slow.  Disable them.
  (flycheck-gometalinter-fast t))
