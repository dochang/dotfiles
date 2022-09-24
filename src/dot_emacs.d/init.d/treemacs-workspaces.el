(req-package treemacs-workspaces

  :ensure treemacs

  :config

  (setq-default treemacs--project-positions (make-hash-table :test #'equal :size 20))
  ;; If we enable `lsp-treemacs' but not enable `treemacs', An error of
  ;; `wrong-type-argument' will be raised because `treemacs--project-positions'
  ;; is nil rather than a hash table.  Initialize it after the library loaded.

  )
