;; https://emacs.stackexchange.com/questions/477/how-do-i-automatically-save-org-mode-buffers
(advice-add 'org-capture :after #'$org-save-all-org-buffers)

(req-package org-capture
  :ensure org-plus-contrib
  :bind (("C-c c" . org-capture)
         ;; For user convenience.
         )
  :hook (org-capture-after-finalize . org-save-all-org-buffers))
