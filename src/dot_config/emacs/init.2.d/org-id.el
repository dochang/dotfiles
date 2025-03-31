(defun $org-id-get-create-on-save ()
  (org-map-entries 'org-id-get-create))
;; https://stackoverflow.com/a/16247032

(setup org-id
  (:package org)
  (:when-loaded
    (:option org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
    (:option org-id-track-globally t)
    ;; Required by org-brain.
    )
  )
