;; https://emacs.stackexchange.com/questions/477/how-do-i-automatically-save-org-mode-buffers
(advice-add 'org-agenda :after #'$org-save-all-org-buffers)

(defun $org-agenda-mode-hook ()
  "Do not wrap lines in `org-agenda-mode'."
  (visual-line-mode -1)
  (toggle-truncate-lines 1))

(req-package org-agenda
  :ensure org-plus-contrib
  :bind (("C-c a" . org-agenda)
         ;; For user convenience.
         )
  :hook ((org-agenda-mode . $org-agenda-mode-hook)
         (org-agenda-finalize . org-save-all-org-buffers)))
