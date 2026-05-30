(setup org-clock
  (:package org)

  (:when-loaded

    ;; Also insert clocking info into the drawer "LOGBOOK".
    (setopt org-clock-into-drawer t)

    )

  )
