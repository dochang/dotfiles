(req-package org-clock
  :ensure org-plus-contrib
  :custom
  ;; Also insert clocking info into the drawer "LOGBOOK".
  (org-clock-into-drawer t)
  ;; Resolve open clocks if we're idle more than 5 mins.
  (org-clock-idle-time 5)
  ;; Save the running clock when Emacs is closed.  The clock is
  ;; resumed when Emacs restarts.
  (org-clock-persist 'clock)
  )
