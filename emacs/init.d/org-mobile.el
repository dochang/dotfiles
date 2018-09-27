(req-package org-mobile

  :ensure org-plus-contrib

  :after org
  ;; `:after' is required because `org-mobile-inbox-for-pull' depends on
  ;; `org-directory', which must be set before setting
  ;; `org-mobile-inbox-for-pull'.

  :custom

  ;; Files to be staged for MobileOrg
  (org-mobile-files '(org-agenda-files))

  ;; The file where captured notes and flags from MobileOrg will be
  ;; appended to.
  (org-mobile-inbox-for-pull
   (expand-file-name "from-mobile.org" org-directory))

  )
