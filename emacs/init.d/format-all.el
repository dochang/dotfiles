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

  ;; toml
  (puthash 'prettier
           "npm install --global prettier prettier-plugin-toml"
           format-all--install-table)
  (format-all--pushhash 'toml-mode
                        `(prettier . ,(lambda () "toml"))
                        format-all--mode-table)

  ;; jsonnet
  (puthash 'jsonnet "jsonnet" format-all--executable-table)
  (puthash 'jsonnet "brew install jsonnet" format-all--install-table)
  (format-all--pushhash 'jsonnet-mode
                        `(jsonnet . ,(lambda () "jsonnet"))
                        format-all--mode-table)
  (puthash 'jsonnet
           (lambda (executable mode-result)
             (ignore mode-result)
             (format-all--buffer-easy executable "fmt" "-"))
           format-all--format-table)

  )
