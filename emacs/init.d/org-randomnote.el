(req-package org-randomnote
  :after org
  ;; Configure org-randomnote after org as `org-agenda-files' is needed.
  :commands (org-randomnote)
  :custom
  (org-randomnote-candidates (org-agenda-files)))
