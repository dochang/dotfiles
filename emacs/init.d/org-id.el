(req-package org-id
  :ensure org-plus-contrib
  :init
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))
