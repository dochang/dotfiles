(req-package editorconfig
  :hook (emacs-startup . editorconfig-mode)
  :init
  (setq editorconfig-get-properties-function
        'editorconfig-get-properties)
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :config
  (add-to-list 'editorconfig-indentation-alist
               '(json-mode js-indent-level json-reformat:indent-width))
  (add-to-list 'editorconfig-indentation-alist
               '(nxml-mode nxml-child-indent nxml-attribute-indent))
  (add-to-list 'editorconfig-indentation-alist
               '(puppet-mode puppet-indent-level puppet-include-indent)))
