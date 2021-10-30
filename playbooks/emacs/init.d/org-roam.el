(eval-and-compile
  (require 'req-package))

(define-advice org-roam--list-all-files (:before (&rest r) ensure-org-roam-directory-exists)
  (make-directory org-roam-directory 'parents))

(req-package org-roam

  :require hydra

  :after org
  ;; `:after' is required because `org-brain-path' depends on `org-directory',
  ;; which must be set before setting `org-brain-path'.

  :hook ((emacs-startup . org-roam-mode))

  :init

  (setq org-roam-directory (expand-file-name "roam" org-directory))

  (setq org-roam-v2-ack t)
  ;; Suppress org-roam upgrade warning.

  (defhydra $hydra-org-roam (:color teal)
    "org-roam"
    ("i" org-roam-insert "insert")
    ("l" org-roam "roam")
    ("f" org-roam-find-file "find file")
    ("g" org-roam-show-graph "show graph"))

  )
