(req-package format-all

  :hook (((prog-mode conf-mode) . format-all-mode)
         ((prog-mode conf-mode) . format-all-ensure-formatter))

  :config

  ;; Add isort for python-mode
  ;;
  ;; Note, currently format-all doesn't support multiple formatters for one
  ;; buffer.
  ;;
  ;; https://github.com/lassik/emacs-format-all-the-code/issues/6
  ;;
  ;; No need to remove black from `format-all--language-table'.  The last
  ;; defined will be the first.
  (define-format-all-formatter isort-black
    (:executable "/bin/sh")
    (:install "pip install isort black")
    (:languages "Python")
    (:format (format-all--buffer-easy
              executable "-c"
              (format "isort --apply - | black -q %s -"
                      (if (format-all--buffer-extension-p "pyi") "--pyi" "")))))

  (define-format-all-formatter goimports-gofmt
    (:executable "/bin/sh")
    (:install
     (macos "brew install go")
     (windows "scoop install go")
     "go get golang.org/x/tools/cmd/goimports")
    (:languages "Go")
    (:format (format-all--buffer-easy executable "-c" "goimports | gofmt -s")))
  ;; This formatter is wrong.  When formatting invalid code, `goimports'
  ;; outputs nothing and exits with non-zero.  But `gofmt' treats empty input
  ;; as valid code then exits with zero.  This breaks
  ;; `format-all--buffer-easy'.

  (remhash "Go" format-all--language-table)

  (define-format-all-formatter shfmt
    (:executable "shfmt")
    (:install
     (macos "brew install shfmt")
     (windows "scoop install shfmt"))
    (:languages "Shell")
    (:format (format-all--buffer-easy executable)))

  )
