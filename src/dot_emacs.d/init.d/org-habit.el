(req-package org-habit

  :ensure org

  :init

  ;; Show habits in agenda buffers.
  (setq org-habit-show-habits t)

  ;; Show habits for future days.
  (setq org-habit-show-habits-only-for-today nil)

  ;; Do not show the consistency graph of the habits, which are not scheduled,
  ;; on today's agenda.
  (setq org-habit-show-all-today nil)

  )
