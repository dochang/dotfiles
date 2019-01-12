(req-package editorconfig
  :hook (emacs-startup . editorconfig-mode)
  :custom
  (editorconfig-get-properties-function
   'editorconfig-get-properties)
  :config
  (add-to-list 'editorconfig-indentation-alist
               '(nxml-mode nxml-child-indent nxml-attribute-indent))
  (add-to-list 'editorconfig-indentation-alist
               '(puppet-mode puppet-indent-level puppet-include-indent)))
