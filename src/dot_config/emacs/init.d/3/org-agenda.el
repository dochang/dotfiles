;; https://emacs.stackexchange.com/questions/477/how-do-i-automatically-save-org-mode-buffers
(advice-add 'org-agenda :after '$org-save-all-org-buffers)

(defun $org-agenda-mode-hook ()
  ;; Do not wrap lines in `org-agenda-mode'.
  (visual-line-mode -1)
  (toggle-truncate-lines 1))

(setup org-agenda
  (:package org)

  (keymap-global-set "C-c a" 'org-agenda)
  ;; For user convenience.

  (:with-mode (org-agenda-mode)
    (:hook $org-agenda-mode-hook))
  (add-hook 'org-agenda-finalize-hook 'org-save-all-org-buffers)

  (:when-loaded

    ;; Show only the first future repeat of repeated entries.
    (setopt org-agenda-show-future-repeats 'next)

    ;; Show a repeated entry at its latest repeat date, not its base date.
    (setopt org-agenda-prefer-last-repeat t)

    ;; Show deadline delay only after deadline in agenda.  Hide scheduled delay.
    (setopt org-agenda-skip-scheduled-delay-if-deadline nil)
    ;; `post-deadline' suppresses all delays before deadline.  The reason is
    ;; not found.  Use nil instead.

    ;; Hide scheduled entry in agenda if the entry has been done.
    (setopt org-agenda-skip-scheduled-if-done t)

    ;; Hide scheduled entry in agenda if deadline is shown and the scheduled is
    ;; before the deadline.
    (setopt org-agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline)

    ;; Hide timestamp entry in agenda if the entry has been done.
    (setopt org-agenda-skip-timestamp-if-done t)

    ;; Hide timestamp entry in agenda if deadline is shown.
    (setopt org-agenda-skip-timestamp-if-deadline-is-shown t)

    ;; Hide deadline entry in agenda if the entry has been done.
    (setopt org-agenda-skip-deadline-if-done t)

    ;; Hide deadline prewarning if scheduled is shown and the prewarning is
    ;; before the scheduled.
    (setopt org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

    ;; Display all timestamps even if they are in the same day.  We have to do
    ;; this.  Otherwise, the timestamps in the same day but with different times
    ;; will be merged into the first one, and others will disappear in the
    ;; agenda.
    (setopt org-agenda-skip-additional-timestamps-same-entry nil)

    )

  )
