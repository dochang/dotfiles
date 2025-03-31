(setup org-clock
  (:package org)

  (:when-loaded

    ;; Also insert clocking info into the drawer "LOGBOOK".
    (:option org-clock-into-drawer t)

    )

  )
