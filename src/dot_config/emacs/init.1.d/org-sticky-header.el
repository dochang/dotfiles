(setup (:package org-sticky-header)

  (:with-mode (org-mode)
    (:hook org-sticky-header-mode))

  (:when-loaded
    (:option org-sticky-header-full-path 'full)
    )

  )
