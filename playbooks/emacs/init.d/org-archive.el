(mapc (lambda (f)
        (advice-add f :after #'$org-save-all-org-buffers))
      '(org-archive-subtree-default
        org-archive-subtree-default-with-confirmation
        org-archive-subtree
        org-archive-to-archive-sibling
        org-archive-set-tag))

(req-package org-archive
  :ensure org-plus-contrib
  :hook (org-archive . org-save-all-org-buffers))
