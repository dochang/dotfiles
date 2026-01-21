;; https://emacs.stackexchange.com/questions/477/how-do-i-automatically-save-org-mode-buffers

(setup org-archive
  (:package org)

  (add-hook 'org-archive-hook #'org-save-all-org-buffers)

  (:when-loaded

    (mapc (lambda (f)
            (unless (advice-member-p #'$org-save-all-org-buffers f)
              (advice-add f :after #'$org-save-all-org-buffers)))
          '(org-archive-subtree-default
            org-archive-subtree-default-with-confirmation
            org-archive-subtree
            org-archive-to-archive-sibling
            org-archive-set-tag))

    )

  )
