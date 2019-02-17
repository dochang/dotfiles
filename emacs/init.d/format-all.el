(req-package format-all

  :hook (prog-mode . format-all-mode)

  :config

  (puthash 'yaml-mode
           `((prettier . ,(lambda () "yaml")))
           format-all-mode-table)
  ;; Use prettier to format yaml files instead of yq.
  ;;
  ;; https://github.com/mikefarah/yq/issues/25

  )
