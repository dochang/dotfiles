(req-package format-all

  :hook ((prog-mode conf-mode) . format-all-mode)

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

  )
