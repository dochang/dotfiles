(setup org-archive

  (add-hook 'org-archive-hook 'org-save-all-org-buffers)

  (:when-loaded

    (mapc (lambda (f)
            (advice-add f :after #'$org-save-all-org-buffers))
          '(org-archive-subtree-default
            org-archive-subtree-default-with-confirmation
            org-archive-subtree
            org-archive-to-archive-sibling
            org-archive-set-tag))

    )

  )
