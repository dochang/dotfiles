(req-package org-link-minor-mode
  :ensure (org-link-minor-mode :pin :quelpa)
  :quelpa (org-link-minor-mode
           :fetcher github
           :repo "seanohalpin/org-link-minor-mode")
  :hook ((emacs-lisp-mode) . org-link-minor-mode))
