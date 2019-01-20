(req-package org-table-sticky-header
  :hook ((org-mode . org-table-sticky-header-mode)
         (org-table-sticky-header-mode . $org-fix-header-line-format))
  :config
  (make-variable-buffer-local 'org-table-sticky-header--last-win-start)
  (make-variable-buffer-local 'org-table-sticky-header--old-header-line-format)
  ;; They should be buffer-local because they keep buffer state.
  )
