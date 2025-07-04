;;; Org

(defun $org-created-get-create-on-save ()
  (org-map-entries '$org-created-get-create))

(defun $org-created-get-create (&optional force)
  (interactive "P")
  (when force
    (org-entry-put (point) "CREATED" nil))
  ($org-created-get (point) 'create))

(defun $org-created-get (&optional pom create prefix)
  (org-with-point-at pom
    (let ((timestamp (org-entry-get nil "CREATED")))
      (cond
       ((and timestamp
             (stringp timestamp)
             (or (string-match-p "\\[.+\\]" timestamp)
                 (string-match-p "<.+>" timestamp))
             ;; Must support i18n day name.
             )
        timestamp)
       (create
        (setq timestamp (format-time-string (org-time-stamp-format 'long 'inactive)))
        (org-entry-put pom "CREATED" timestamp)
        timestamp)))))

(defun $org-mode-hook ()
  (smartparens-mode -1)
  ;; Disable smartparens-mode.  It notably retards org buffers.  The reason is
  ;; still unknown.

  ;; `visual-line-mode-map' and `smartparens-strict-mode-map' overrides
  ;; `org-mode-map'.  Rebind some org-mode commands.
  (mapc (lambda (map)
          (local-set-key (car map) (cdr map)))
        (mapcar (lambda (map)
                  (cons (kbd (car map)) (cdr map)))
                '(("C-a" . org-beginning-of-line)
                  ("C-d" . org-delete-char)
                  ("C-e" . org-end-of-line)
                  ("C-k" . org-kill-line))))
  (add-hook 'before-save-hook '$org-id-get-create-on-save nil 'local)
  (add-hook 'before-save-hook '$org-created-get-create-on-save nil 'local))

(define-advice org-read-agenda-file-list (:before () ensure-created)
  (when (and (stringp org-agenda-files)
             (not (file-exists-p org-agenda-files)))
    (let ((dir (file-name-directory org-agenda-files)))
      (unless (file-exists-p dir)
        (make-directory dir t)))
    (write-region "" nil org-agenda-files t nil nil 'excl)))

(defun $org-load-hook ()
  (org-clock-persistence-insinuate)
  (setopt org-modules
          (seq-reduce
           (lambda (mods mod)
             (if (seq-contains-p mods mod)
                 mods
               (cons mod mods)))
           '(org-id
             ;; [[info:org#Handling%20links]]
             ;;
             ;; We must load `org-id' before using `org-store-link'.
             org-habit
             ;; Track habits.
             )
           org-modules))
  (setopt org-export-backends
          (seq-reduce
           (lambda (backends backend)
             (if (seq-contains-p backends backend)
                 backends
               (cons backend backends)))
           '(md)
           org-export-backends)))

;; Emacs runs `org-load-hook' right after loading `org', even before
;; `eval-after-load'.  So we better add the hook function to `org-load-hook'
;; once this file loaded.
(add-hook 'org-load-hook '$org-load-hook)

;; They have to be set before org.el is loaded.
;; To make the change effective, restart emacs.
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

;; https://emacs.stackexchange.com/questions/477/how-do-i-automatically-save-org-mode-buffers
(defun $org-save-all-org-buffers (&rest args)
  "Ignore all arguments so that this function can be used as advice."
  (org-save-all-org-buffers))

(advice-add 'org-refile :after #'$org-save-all-org-buffers)

(defun $org-fix-header-line-format ()
  (if (bound-and-true-p org-table-sticky-header-mode)
      (progn
        (setq org-table-sticky-header--old-header-line-format
              (if (bound-and-true-p org-sticky-header-mode)
                  org-sticky-header-header-line-format
                nil))
        (save-match-data
          (org-table-sticky-header--fetch-header)))
    (setq header-line-format
          (if (bound-and-true-p org-sticky-header-mode)
              org-sticky-header-header-line-format
            nil))))

(defvar **org-timer**)

(setup (:package org)

  (:file-match "/\\.notes\\'")
  ;; Edit `org-default-notes-file' in org-mode.

  (add-hook 'emacs-startup-hook
            (lambda ()

              (require 'transient)

              (transient-define-prefix $transient-org ()
                "org"
                [
                 ["Global"
                  ("a" "agenda" org-agenda)
                  ("b" "switchb" org-switchb)
                  ("c" "capture" org-capture)
                  ]
                 ["Link"
                  ("l" "store link" org-store-link)
                  ("L" "insert link global" org-insert-link-global)
                  ("o" "open at point global" org-open-at-point-global)
                  ]
                 ]
                )

              (keymap-global-set "C-c o" '$transient-org)

              ))

  (add-hook 'org-after-refile-insert-hook 'org-save-all-org-buffers)
  (:with-mode (org-mode)
    (:hook $org-mode-hook))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (unless (bound-and-true-p **org-timer**)
                (setq **org-timer**
                      (run-at-time nil 3600 'org-agenda-to-appt)))))

  (:when-loaded

    (setopt org-directory (expand-file-name "~/org"))

    ;; Skip non-reachable files in `org-agenda-files'.
    (setopt org-agenda-skip-unavailable-files t)

    (setopt org-agenda-files (locate-user-emacs-file "org-agenda-files"))

    ;; This variable needs to be set before org.el is loaded.  If you need to
    ;; make a change while Emacs is running, use the customize interface or
    ;; run the following code after updating it:
    ;;
    ;;   `\\[org-element-update-syntax]'
    (setopt org-list-allow-alphabetical t)

    ;; Insert the first line setting Org-mode in an empty file if the
    ;; filename doesn't automatically trigger Org-mode.
    (setopt org-insert-mode-line-in-empty-file t)

    ;; Don't split the line at the cursor position when creating a new
    ;; headline/item
    (setopt org-M-RET-may-split-line
            '((headline . nil) (item . nil) (default . t)))

    ;; Fontify code in code blocks.
    (setopt org-src-fontify-natively t)

    ;; Store relative pathname in links for files in the current directory and
    ;; subdirectories of it.  Store absolute pathname in links for other files.
    (setopt org-link-file-path-type 'adaptive)

    ;; Leave a blank line before new heading/item.
    (setopt org-blank-before-new-entry
            '((heading . auto) (plain-list-item . auto)))

    ;; Adjust the level when yanking subtrees.
    (setopt org-yank-adjusted-subtrees t)

    ;; Make `C-a' & `C-e' behave specially in headlines & items.
    (setopt org-special-ctrl-a/e t)

    ;; Make `C-k' behave specially in headlines.
    (setopt org-special-ctrl-k t)

    ;; Turn on `org-indent-mode' on startup.
    (setopt org-startup-indented t)

    ;; Record a note when entering each TODO state.
    (setopt org-todo-keywords '((type "NEXT(n@)" "TODO(t@)" "|" "DONE(d@)")))

    ;; Insert state change notes into the drawer "LOGBOOK".
    (setopt org-log-into-drawer t)

    ;; Prompt for a note when a task moves to the DONE state.
    (setopt org-log-done 'note)

    ;; Record a note when clocking out of an item.
    (setopt org-log-note-clock-out t)

    ;; Record when the deadline date of a tasks is modified.
    (setopt org-log-redeadline 'note)

    ;; Record when the scheduling date of a tasks is modified.
    (setopt org-log-reschedule 'note)

    ;; Prompt for a note when a task is refiled.
    (setopt org-log-refile 'note)

    ;; Don't remove the ID properties from clones of a subtree.  Inherit
    ;; the ID property with a new ID instead.
    (setopt org-clone-delete-id nil)

    ;; Allow to create new nodes as refile targets with confirmation.
    (setopt org-refile-allow-creating-parent-nodes 'confirm)

    ;; Allow to refile entries to the top level in the file.
    (setopt org-refile-use-outline-path 'full-file-path)

    ;; Set targets for refiling entries.
    (setopt org-refile-targets
            '((nil . (:maxlevel . 3))
              (org-default-notes-file . (:maxlevel . 3))
              (org-agenda-files . (:maxlevel . 3))))

    ;; Do not limit date range.
    (setopt org-read-date-force-compatible-dates nil)

    ;; Search agenda archives additionally.
    (setopt org-agenda-text-search-extra-files '(agenda-archives))

    (setopt org-archive-location "%s_archive::datetree/")

    (setopt org-deadline-warning-days 0)
    ;; For orgzly.

    ;; Complete the outline path in one step.
    ;;
    ;; When use a completion package like ido or ivy, this variable must be set
    ;; to nil.
    ;;
    ;; https://github.com/abo-abo/swiper/issues/444#issuecomment-286693939
    ;; https://github.com/syl20bnr/spacemacs/issues/3094#issuecomment-142061734
    ;; https://github.com/abo-abo/swiper/issues/1254
    (setopt org-outline-path-complete-in-steps nil)

    (:with-mode (org-sticky-header-mode org-table-sticky-header-mode)
      (:hook $org-fix-header-line-format))

    )

  )
