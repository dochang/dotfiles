(defun $org-agenda-mode-hook ()
  "Do not wrap lines in `org-agenda-mode'."
  (visual-line-mode -1)
  (toggle-truncate-lines 1))

(req-package org-agenda
  :ensure org-plus-contrib
  :hook (org-agenda-mode . $org-agenda-mode-hook))
