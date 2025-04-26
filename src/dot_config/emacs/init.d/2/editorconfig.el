(setup (:package editorconfig)
  (add-hook 'emacs-startup-hook 'editorconfig-mode)
  (:when-loaded
    (setopt editorconfig-get-properties-function 'editorconfig-get-properties)
    (setopt editorconfig-trim-whitespaces-mode
            (if (package-installed-p 'ws-butler)
                'ws-butler-mode
              nil))
    (setopt editorconfig-indentation-alist
            (seq-reduce
             (lambda (alist elem)
               (if (seq-contains-p alist elem)
                   alist
                 (cons elem alist)))
             '(
               (json-mode js-indent-level json-reformat:indent-width)
               (nxml-mode nxml-child-indent nxml-attribute-indent)
               (puppet-mode puppet-indent-level puppet-include-indent)
               (kdl-mode tab-width)
               (kdl-ts-mode kdl-ts-mode-indent-offset)
               (mermaid-mode mermaid-indentation-level)
               (mermaid-ts-mode mermaid-ts-indent-level)
               )
             editorconfig-indentation-alist))
    )
  )
