(req-package org-habit

  :ensure org-plus-contrib

  :custom

  ;; Show habits in agenda buffers.
  (org-habit-show-habits t)

  ;; Show habits for future days.
  (org-habit-show-habits-only-for-today nil)

  ;; Do not show the consistency graph of the habits, which are not scheduled,
  ;; on today's agenda.
  (org-habit-show-all-today nil)

  )
