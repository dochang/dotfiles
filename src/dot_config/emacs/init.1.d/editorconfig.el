(setup (:package editorconfig)
  (add-hook 'emacs-startup-hook 'editorconfig-mode)
  (:when-loaded
    (:option editorconfig-get-properties-function 'editorconfig-get-properties)
    (:option editorconfig-trim-whitespaces-mode
             (if (package-installed-p 'ws-butler)
                 'ws-butler-mode
               nil))
    (add-to-list 'editorconfig-indentation-alist
                 '(json-mode js-indent-level json-reformat:indent-width))
    (add-to-list 'editorconfig-indentation-alist
                 '(nxml-mode nxml-child-indent nxml-attribute-indent))
    (add-to-list 'editorconfig-indentation-alist
                 '(puppet-mode puppet-indent-level puppet-include-indent))
    (add-to-list 'editorconfig-indentation-alist
                 '(kdl-mode tab-width))
    )
  )
