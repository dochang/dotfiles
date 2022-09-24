(eval-and-compile
  (require 'req-package))

(req-package org-brain

  :after org
  ;; `:after' is required because `org-brain-path' depends on `org-directory',
  ;; which must be set before setting `org-brain-path'.

  :bind (("C-c B" . org-brain-prefix-map))

  :hook (before-save . org-brain-ensure-ids-in-buffer)

  :init

  (setq org-brain-path (expand-file-name "brain" org-directory))

  (setq org-brain-visualize-default-choices 'all)

  (require 'org-brain)

  )
