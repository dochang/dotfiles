(req-package format-all

  :hook ((prog-mode conf-mode) . format-all-mode)

  :config

  ;; Add isort for python-mode
  ;;
  ;; Note, currently format-all doesn't support multiple formatters for one
  ;; buffer.
  ;;
  ;; https://github.com/lassik/emacs-format-all-the-code/issues/6
  (puthash 'isort "isort" format-all--executable-table)
  (puthash 'isort "pip install isort" format-all--install-table)
  (puthash 'black
           (lambda (executable mode-result)
             (ignore mode-result)
             ;; https://github.com/timothycrosley/isort/issues/40
             (format-all--buffer-easy
              "/bin/sh" "-c"
              (format "%s %s %s | %s -q -"
                      (format-all--formatter-executable 'isort)
                      "--apply"
                      "-"
                      executable)))
           format-all--format-table)

  )
