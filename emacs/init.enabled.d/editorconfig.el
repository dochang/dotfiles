(req-package editorconfig
  :init
  (setq editorconfig-get-properties-function
        'editorconfig-core-get-properties-hash)
  :config
  (add-to-list 'editorconfig-indentation-alist '(nxml-mode nxml-child-indent nxml-attribute-indent))
  (add-to-list 'editorconfig-indentation-alist '(puppet-mode puppet-indent-level puppet-include-indent)))
