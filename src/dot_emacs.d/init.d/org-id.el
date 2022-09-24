(defun $org-id-get-create-on-save ()
  (org-map-entries 'org-id-get-create))
;; https://stackoverflow.com/a/16247032

(req-package org-id
  :ensure org
  :init
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq org-id-track-globally t)
  ;; Required by org-brain.
  )
