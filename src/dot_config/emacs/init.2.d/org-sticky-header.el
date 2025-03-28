(setup (:package org-sticky-header)

  (:with-mode (org-mode)
    (:hook org-sticky-header-mode))

  (:when-loaded
    (setopt org-sticky-header-full-path 'full)
    )

  )
