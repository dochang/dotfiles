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
             (string-match-p "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\]" timestamp))
        timestamp)
       (create
        (setq timestamp (format-time-string (org-time-stamp-format 'long 'inactive)))
        (org-entry-put pom "CREATED" timestamp)
        timestamp)))))

(defun $org-mode-hook ()
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

(defun $org-load-hook ()
  ;; `org-cycle-hide-drawers` doesn't work if `org-agenda-files`
  ;; doesn't exist.  It would cause org mode cannot expand headings.
  (when (and (stringp org-agenda-files)
             (not (file-exists-p org-agenda-files)))
    (let ((dir (file-name-directory org-agenda-files)))
      (unless (file-exists-p dir)
        (make-directory dir t)))
    (write-region "" nil org-agenda-files t nil nil 'excl))
  (org-clock-persistence-insinuate)
  ;; [[info:org#Handling%20links]]
  ;;
  ;; We must load `org-id' before using `org-store-link'.
  (add-to-list 'org-modules 'org-id)
  ;; Track habits.
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-export-backends 'md))

;; Emacs runs `org-load-hook' right after loading `org', even before
;; `eval-after-load'.  Also, the `:init' section of `org' may be ran after
;; `org' loaded.  So we have to add the hook function to `org-load-hook' once
;; this file loaded.
;;
;; When `:demand t', the execution order of `use-package' is:
;;
;; :init
;; (require 'org)
;; :hook
(add-hook 'org-load-hook '$org-load-hook)

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

(req-package org
  :ensure org-plus-contrib

  :require hydra

  :mode ("/\\.notes\\'" . org-mode)
  ;; Edit `org-default-notes-file' in org-mode.

  :bind (("C-c o" . $hydra-org/body))

  :hook ((org-after-refile-insert . org-save-all-org-buffers)
         (org-mode . $org-mode-hook)
         (emacs-startup . (lambda ()
                            (unless (bound-and-true-p **org-timer**)
                              (setq **org-timer** (run-at-time nil 3600 'org-agenda-to-appt))))))

  :init

  ;; They have to be set before org.el is loaded.
  ;; To make the change effective, restart emacs.
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)

  ;; This variable needs to be set before org is loaded.  If you
  ;; need to make a change while Emacs is running, use the customize
  ;; interface or run the following code after updating it:
  ;;
  ;;   (when (featurep 'org-element) (load "org-element" t t))
  (setq org-list-allow-alphabetical t)

  (setq org-directory (expand-file-name "~/org"))

  ;; Skip non-reachable files in `org-agenda-files'.
  (setq org-agenda-skip-unavailable-files t)

  (setq org-agenda-files (locate-user-emacs-file "org-agenda-files"))

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

  ;; Record a note when entering each TODO state.
  (setq org-todo-keywords '((type "NEXT(n@)" "TODO(t@)" "|" "DONE(d@)")))

  ;; Insert state change notes into the drawer "LOGBOOK".
  (setq org-log-into-drawer t)

  ;; Prompt for a note when a task moves to the DONE state.
  (setq org-log-done 'note)

  ;; Record a note when clocking out of an item.
  (setq org-log-note-clock-out t)

  ;; Record when the deadline date of a tasks is modified.
  (setq org-log-redeadline 'note)

  ;; Record when the scheduling date of a tasks is modified.
  (setq org-log-reschedule 'note)

  ;; Prompt for a note when a task is refiled.
  (setq org-log-refile 'note)

  ;; Don't remove the ID properties from clones of a subtree.  Inherit
  ;; the ID property with a new ID instead.
  (setq org-clone-delete-id nil)

  ;; Allow to create new nodes as refile targets with confirmation.
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Allow to refile entries to the top level in the file.
  (setq org-refile-use-outline-path 'full-file-path)

  ;; Set targets for refiling entries.
  (setq org-refile-targets
        '((nil . (:maxlevel . 3))
          (org-default-notes-file . (:maxlevel . 3))
          (org-agenda-files . (:maxlevel . 3))))

  ;; Do not limit date range.
  (setq org-read-date-force-compatible-dates nil)

  ;; Search agenda archives additionally.
  (setq org-agenda-text-search-extra-files '(agenda-archives))

  (setq org-archive-location "%s_archive::datetree/")

  (setq org-deadline-warning-days 1)

  ;; Complete the outline path in one step.
  ;;
  ;; When use a completion package like ido or ivy, this variable must be set
  ;; to nil.
  ;;
  ;; https://github.com/abo-abo/swiper/issues/444#issuecomment-286693939
  ;; https://github.com/syl20bnr/spacemacs/issues/3094#issuecomment-142061734
  ;; https://github.com/abo-abo/swiper/issues/1254
  (setq org-outline-path-complete-in-steps nil)

  (defhydra $hydra-org (:color teal)
    "org"
    ("a" org-agenda "agenda")
    ("b" org-iswitchb "iswitchb")
    ("c" org-capture "capture")
    ("C" cfw:open-org-calendar "calendar")
    ("i" org-cliplink "cliplink")
    ("l" org-store-link "store link")
    ("L" org-insert-link-global "insert link global")
    ("n" $hydra-org-roam/body "org-roam")
    ("o" org-open-at-point-global "open at point global")
    ("q" nil "quit")
    ("r" org-randomnote "random note")))
