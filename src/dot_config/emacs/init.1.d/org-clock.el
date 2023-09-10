(setup org-clock

  (:when-loaded

    ;; Also insert clocking info into the drawer "LOGBOOK".
    (:option org-clock-into-drawer t)

    ;; Resolve open clocks if we're idle more than 5 mins.
    (:option org-clock-idle-time 5)

    ;; Save the running clock when Emacs is closed.  The clock is
    ;; resumed when Emacs restarts.
    (:option org-clock-persist 'clock)

    )

  )
