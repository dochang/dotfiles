(req-package org-sticky-header
  :hook ((org-mode . org-sticky-header-mode)
         (org-sticky-header-mode . $org-fix-header-line-format))
  :custom
  (org-sticky-header-full-path 'full))
