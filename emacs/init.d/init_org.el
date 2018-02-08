;;; Org

(defun $diary-chinese-date (month day &optional year mark)
  "Specific date(s) diary entry.
Entry applies if date is MONTH, DAY, YEAR.  Each parameter can be a
list of integers, t (meaning all values), or an integer.  The order
of the input parameters changes according to `calendar-date-style'
\(e.g. to DAY MONTH YEAR in the European style).

YEAR can also be nil, which means all values like t.

An optional parameter MARK specifies a face or single-character string
to use when highlighting the day in the calendar."
  ;; Load `cal-china' for `calendar-chinese-from-absolute'.
  (when (require 'cal-china nil t)
    (pcase-let* ((ddate (diary-make-date month day year))
                 (`(,dy ,dm ,dd)
                  (list
                   (calendar-extract-year ddate)
                   (calendar-extract-month ddate)
                   (calendar-extract-day ddate)))
                 (`(,cc ,cy ,m ,d)      ; current chinese date
                  (calendar-chinese-from-absolute
                   (calendar-absolute-from-gregorian
                    (or date (calendar-current-date)))))
                 (y (+ (* cc 100) cy))
                 ;; The chinese year is (+ (* cc 100) cy) actually.
                 ;; See `calendar-chinese-from-absolute-for-diary' for details.
                 )
      (and
       (or (and (listp dd) (memq d dd))
           (equal d dd)
           (eq dd t))
       (or (and (listp dm) (memq m dm))
           (equal m dm)
           (eq dm t))
       (or (not dy)
           (and (listp dy) (memq y dy))
           (equal y dy)
           (eq dy t))
       (cons mark entry)))))

(defun $diary-chinese-hair-cutting-date (day)
  (or ($diary-chinese-date 1 '())
      ($diary-chinese-date 2 '(8 day))  ; 8 = 2 + 6
      ($diary-chinese-date (number-sequence 3 12) day)))

(defun $org-load-hook ()
  (setq org-directory (expand-file-name "~/org"))
  ;; Skip non-reachable files in `org-agenda-files'.
  (setq org-agenda-skip-unavailable-files t)
  (let ((default-directory org-directory))
    ;; Files to be staged for MobileOrg
    (setq org-mobile-files '(org-agenda-files))
    ;; The file where captured notes and flags from MobileOrg will be
    ;; appended to.
    (setq org-mobile-inbox-for-pull (expand-file-name "from-mobile.org"))
    (setq org-agenda-files (locate-user-emacs-file "org-agenda-files")))
  ;; `org-cycle-hide-drawers` doesn't work if `org-agenda-files`
  ;; doesn't exist.  It would cause org mode cannot expand headings.
  (when (and (stringp org-agenda-files)
             (not (file-exists-p org-agenda-files)))
    (let ((dir (file-name-directory org-agenda-files)))
      (unless (file-exists-p dir)
        (make-directory dir t)))
    (write-region "" nil org-agenda-files t nil nil 'excl))
  ;; Insert the first line setting Org-mode in an empty file if the
  ;; filename doesn't automatically trigger Org-mode.
  (setq org-insert-mode-line-in-empty-file t)
  ;; Don't split the line at the cursor position when creating a new
  ;; headline/item
  (setq org-M-RET-may-split-line '((headline . nil) (item . nil) (default . t)))
  ;; Fontify code in code blocks.
  (setq org-src-fontify-natively t)
  ;; Store relative pathname in links for files in the current directory and
  ;; subdirectories of it.  Store absolute pathname in links for other files.
  (setq org-link-file-path-type 'adaptive)
  ;; Leave a blank line before new heading/item.
  (setq org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
  ;; Adjust the level when yanking subtrees.
  (setq org-yank-adjusted-subtrees t)
  ;; Make `C-a' & `C-e' behave specially in headlines & items.
  (setq org-special-ctrl-a/e t)
  ;; Make `C-k' behave specially in headlines.
  (setq org-special-ctrl-k t)
  ;; Turn on `org-indent-mode' on startup.
  (setq org-startup-indented t)
  ;; Place footnotes locally at the end of the current outline node.
  (setq org-footnote-section nil)
  ;; Record a note when entering each TODO state.
  (setq org-todo-keywords '((type "NEXT(n@)" "TODO(t@)" "|" "DONE(d@)")))
  ;; Insert state change notes into the drawer "LOGBOOK".
  (setq org-log-into-drawer t)
  ;; Also insert clocking info into the drawer "LOGBOOK".
  (setq org-clock-into-drawer t)
  ;; Prompt for a note when a task moves to the DONE state.
  (setq org-log-done 'note)
  ;; Record a note when clocking out of an item.
  (setq org-log-note-clock-out t)
  ;; Record when the deadline date of a tasks is modified.
  (setq org-log-redeadline 'note)
  ;; Record when the scheduling date of a tasks is modified.
  (setq org-log-reschedule 'note)
  ;; Resolve open clocks if we're idle more than 5 mins.
  (setq org-clock-idle-time 5)
  ;; Save the running clock when Emacs is closed.  The clock is
  ;; resumed when Emacs restarts.
  (setq org-clock-persist 'clock)
  (org-clock-persistence-insinuate)
  ;; Prompt for a note when a task is refiled.
  (setq org-log-refile 'note)
  ;; Use `fundamental-mode' in `#+BEGIN_COMMENT' ... `#+END_COMMENT'.
  (setq org-edit-src-region-extra
        '(("^[ \t]*#\\+begin_comment.*\n" "\n[ \t]*#\\+end_comment" "fundamental")
          ("<comment>[ \t]*\n?" "\n?[ \t]*</comment>" "fundamental")))
  ;; Preserve leading whitespace characters on export and when
  ;; switching between the org buffer and the language mode edit
  ;; buffer.  This variable is especially useful for tangling
  ;; languages such as Python or Makefile, in which whitespace
  ;; indentation in the output is critical.
  (setq org-src-preserve-indentation t)
  ;; Don't indent for the content of a source code block.  NOTE: It
  ;; has no effect if `org-src-preserve-indentation' is non-nil.  But
  ;; we still set it to 0 here.
  (setq org-edit-src-content-indentation 0)
  ;; [[info:org#Handling%20links]]
  ;;
  ;; We must load `org-id' before using `org-store-link' since
  ;; `org-link-to-org-use-id' depends on `org-id'.
  ;;
  ;; See `org-link-to-org-use-id'.
  (add-to-list 'org-modules 'org-id)
  ;; Don't remove the ID properties from clones of a subtree.  Inherit
  ;; the ID property with a new ID instead.
  (setq org-clone-delete-id nil)
  ;; Allow to create new nodes as refile targets with confirmation.
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  ;; Allow to refile entries to the top level in the file.
  (setq org-refile-use-outline-path 'file)
  ;; Set targets for refiling entries.
  (setq org-refile-targets
        '((nil . (:maxlevel . 3))
          (org-default-notes-file . (:maxlevel . 3))
          (org-agenda-files . (:maxlevel . 3))))
  ;; Templates for the creation of new entries.
  (setq org-capture-templates
        '(("*" "NOTE" entry (file+olp+datetree "")
           "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%a"
           :empty-lines 1)
          ("n" "NEXT" entry (file+olp+datetree "")
           "* NEXT %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%a"
           :empty-lines 1)
          ("t" "TODO" entry (file+olp+datetree "")
           "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%a"
           :empty-lines 1)
          ("o" "CLOCK CAPTURE")
          ("o*" "CLOCK NOTE" entry (clock)
           "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%a"
           :empty-lines 1)
          ("on" "CLOCK NEXT" entry (clock)
           "* NEXT %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%a"
           :empty-lines 1)
          ("ot" "CLOCK TODO" entry (clock)
           "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%a"
           :empty-lines 1)
          ("oi" "CLOCK PLAIN LIST ITEM" item (clock)
           nil)
          ("oc" "CLOCK CHECKBOX ITEM" checkitem (clock)
           nil)
          ("oT" "CLOCK TABLE LINE" table-line (clock)
           nil
           :empty-lines 1)
          ("o " "CLOCK PLAIN TEXT" plain (clock)
           "%?"
           :empty-lines 1)))
  ;; Use the date at point when capturing from agendas.
  (setq org-capture-use-agenda-date t)
  ;; Do not limit date range.
  (setq org-read-date-force-compatible-dates nil)
  ;; Show only one occurrence of a repeating timestamp in the agenda.
  (setq org-agenda-repeating-timestamp-show-all nil)
  ;; Show deadline delay only after deadline in agenda.  Hide scheduled delay.
  (setq org-agenda-skip-scheduled-delay-if-deadline 'post-deadline)
  ;; Hide scheduled entry in agenda if the entry has been done.
  (setq org-agenda-skip-scheduled-if-done t)
  ;; Hide scheduled entry in agenda if deadline is shown and the scheduled is
  ;; before the deadline.
  (setq org-agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline)
  ;; Hide timestamp entry in agenda if the entry has been done.
  (setq org-agenda-skip-timestamp-if-done t)
  ;; Hide timestamp entry in agenda if deadline is shown.
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  ;; Hide deadline entry in agenda if the entry has been done.
  (setq org-agenda-skip-deadline-if-done t)
  ;; Hide deadline prewarning if scheduled is shown and the prewarning is
  ;; before the scheduled.
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  ;; Display all timestamps even if they are in the same day.  We have to do
  ;; this.  Otherwise, the timestamps in the same day but with different times
  ;; will be merged into the first one, and others will disappear in the
  ;; agenda.
  (setq org-agenda-skip-additional-timestamps-same-entry nil)
  ;; Search agenda archives additionally.
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  ;; Track habits.
  (add-to-list 'org-modules 'org-habit)
  ;; Show habits in agenda buffers.
  (setq org-habit-show-habits t)
  ;; Show habits for future days.
  (setq org-habit-show-habits-only-for-today nil)
  ;; Do not show the consistency graph of the habits, which are not scheduled,
  ;; on today's agenda.
  (setq org-habit-show-all-today nil))

(defun $org-mode-hook ()
  (linum-mode 1)
  (setq truncate-lines nil))

(defvar **org-timer**)

(req-package org-plus-contrib
  :mode ("/\\.notes\\'" . org-mode)
  ;; Edit `org-default-notes-file' in org-mode.

  :bind (("C-c o" . $hydra-org/body)
         ;; Bind the following keys for convenience.
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))

  :init

  ;; They have to be set before org.el is loaded.
  ;; To make the change effective, restart emacs.
  (setq org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t)

  ;; This variable needs to be set before org is loaded.  If you
  ;; need to make a change while Emacs is running, use the customize
  ;; interface or run the following code after updating it:
  ;;
  ;;   (when (featurep 'org-element) (load "org-element" t t))
  (setq org-list-allow-alphabetical t)

  (defhydra $hydra-org (:color teal)
    "org"
    ("a" org-agenda "agenda")
    ("b" org-iswitchb "iswitchb")
    ("c" org-capture "capture")
    ("C" cfw:open-org-calendar "calendar")
    ("l" org-store-link "store link")
    ("L" org-insert-link-global "insert link global")
    ("o" org-open-at-point-global "open at point global")
    ("q" nil "quit"))

  (add-hook 'org-mode-hook '$org-mode-hook)

  ;; Emacs runs `org-load-hook' right after loading `org', before
  ;; `eval-after-load'.  We have to config it before loading `org'.
  (add-hook 'org-load-hook '$org-load-hook))
