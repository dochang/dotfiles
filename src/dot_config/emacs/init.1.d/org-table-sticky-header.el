(setup (:package org-table-sticky-header)

  (:with-mode (org-mode)
    (:hook org-table-sticky-header-mode))

  (:when-loaded

    (make-variable-buffer-local 'org-table-sticky-header--last-win-start)
    (make-variable-buffer-local 'org-table-sticky-header--old-header-line-format)
    ;; They should be buffer-local because they keep buffer state.

    )

  )
