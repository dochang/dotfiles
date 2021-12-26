(req-package org-clock
  :ensure org
  :init
  ;; Also insert clocking info into the drawer "LOGBOOK".
  (setq org-clock-into-drawer t)
  ;; Resolve open clocks if we're idle more than 5 mins.
  (setq org-clock-idle-time 5)
  ;; Save the running clock when Emacs is closed.  The clock is
  ;; resumed when Emacs restarts.
  (setq org-clock-persist 'clock)
  )
