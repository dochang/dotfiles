;; Require `cl-lib'.
;;
;; Do not require `cl`.  It defines functions without the 'cl-' prefix, that
;; pollutes the namespace.
;;
;; - [[info:cl#Usage]]
;; - [[info:cl#Organization]]
(require 'cl-lib nil t)

(random t)

(load (locate-user-emacs-file "packages") t)

(defun $clean-up-user-packages ()
  (interactive)
  (mapc (lambda (pkg)
          (ignore-errors (package-delete pkg)))
        (apply 'append (mapcar 'cddr package-alist))))

(mapc 'load-file
      (let ((etcdir (expand-file-name "~/local/etc/emacs/site-start.d")))
        (and (file-accessible-directory-p etcdir)
             (directory-files etcdir t "^[0-9][0-9].*\\.elc?$"))))

(defvar **default-load-path**
  (copy-sequence load-path))

(defvar **default-custom-theme-load-path**
  (copy-sequence custom-theme-load-path))

(defun $subdirs-to-list (default-directory)
  (when (file-directory-p default-directory)
    (let ((normal-top-level-add-subdirs-inode-list (list))
          (load-path (list nil)))
      (append
       (copy-sequence (normal-top-level-add-to-load-path (list ".")))
       (normal-top-level-add-subdirs-to-load-path)))))

(setq load-path
      (apply 'append
             ($subdirs-to-list "~/local/share/emacs/site-lisp/")
             **default-load-path**
             ;; Enable contributed extensions to org-mode on Debian
             (list "/usr/share/org-mode/lisp")
             (mapcar '$subdirs-to-list
                     (mapcar 'locate-user-emacs-file
                             '("site-lisp/")))))

(setq custom-theme-load-path
      (apply 'append
             **default-custom-theme-load-path**
             (mapcar '$subdirs-to-list
                     (mapcar 'locate-user-emacs-file
                             '()))))


;;; Specify custom file, but not load it.
;;; [[info:emacs#Saving%20Customizations]]
(setq custom-file (locate-user-emacs-file ".emacs-custom.el"))


;;; Customizing `safe-local-variable-values`.
(defvar **default-safe-local-variable-values**
  (copy-alist safe-local-variable-values))

(setq safe-local-variable-values
      (cons (cons 'buffer-auto-save-file-name nil)
            **default-safe-local-variable-values**))


;;; Disable auto save recovery record.
;;; [[info:emacs#Recover]]
(setq auto-save-list-file-prefix nil)


;;; * Mail Config
;;;
;;; ** How to compute the mail address?
;;;
;;;    - [[info:emacs#Mail%20Headers]]
;;;    - [[info:message#News%20Headers]]
;;;
;;; *** User Full Name
;;;
;;;     1. =(user-full-name)=
;;;
;;; *** Domain Part
;;;
;;;     1. =message-user-fqdn=
;;;     2. =(system-name)=
;;;     3. =mail-host-address=
;;;     4. =message-user-mail-address= (i.e. =user-mail-address=)
;;;
;;; *** Mail Address
;;;
;;;     1. =user-mail-address=
;;;
;;;        1. $EMAIL
;;;        2. =(user-login-name)= @ =mail-host-address=
;;;        3. =(user-login-name)= @ =(system-name)=
;;;
;;;           1. =(system-name)= returns =system-name=
;;;
;;; ** API
;;;
;;; *** [[info:elisp#User%20Identification]]
;;;
;;;     - =(user-full-name)=
;;;
;;;       Returns =user-full-name=, which is initialized by =$NAME=
;;;       or a system-defined value.
;;;
;;;     - =user-mail-address=
;;;
;;;     - =(user-login-name)=
;;;
;;;       Returns =user-login-name=.
;;;
;;; *** [[info:elisp#System%20Environment]]
;;;
;;;     - =mail-host-address=
;;;
;;;     - =(system-name)=
;;;
;;;       Returns =system-name=.
;;;
;;; ** NOTES
;;;
;;; 1. Setting `message-send-mail-function' is unnecessary.  Emacs can
;;;    choose a suitable default value for it.
;;;
;;;    [[info:message#Mail%20Variables]]
;;;
(setq user-full-name (or (getenv "NAME") "{{ dotfiles_emacs_name }}")
      user-mail-address (or (getenv "EMAIL") "{{ dotfiles_emacs_email }}")
      mail-from-style 'angles
      ;; Insert BCC to self in messages to be sent.
      mail-self-blind t
      ;; Use the From: header for the envelope-from when sending mail.  Do not
      ;; use `user-mail-address'.
      mail-specify-envelope-from t
      mail-envelope-from 'header
      ;; `compose-mail' will warn if `mail-self-blind' &
      ;; `compose-mail-user-agent-warnings' are both set to `t'.
      ;;
      ;; Suppress this warning.
      compose-mail-user-agent-warnings nil
      ;; Send mail using `sendmail'.
      ;; [[info:emacs#Mail%20Sending]]
      send-mail-function 'sendmail-send-it
      ;; Use `message-user-agent' for mail composition.
      ;; [[info:emacs#Mail%20Methods]]
      mail-user-agent 'message-user-agent)


(defun $trim (s)
  "Remove whitespace at beginning and end of string."
  (if (string-match "\\`[ \t\n\r]+" s) (setq s (replace-match "" t t s)))
  (if (string-match "[ \t\n\r]+\\'" s) (setq s (replace-match "" t t s)))
  s)

(defun $funcall-when (arg fn)
  (and arg (funcall fn arg)))

(defmacro $andp (predicate)
  (let ((object (cl-gensym))
        (pred (cl-gensym)))
    `(lambda (,object)
       (let ((,pred ,predicate))
         (and (funcall ,pred ,object) ,object)))))

(defun $uuid ()
  "Return an UUID."
  (cond ((seq-reduce '$funcall-when
                     (list
                      ($andp 'file-readable-p)
                      (lambda (uuid-file)
                        (with-temp-buffer
                          (let ((uuid-len 36))
                            (and (<= uuid-len (nth 1 (insert-file-contents uuid-file)))
                                 (buffer-substring (point-min) (+ (point-min) uuid-len))))))
                      ($andp '$uuidgen-p))
                     "/proc/sys/kernel/random/uuid"))
        ((seq-reduce '$funcall-when
                     (list
                      'executable-find
                      'shell-command-to-string
                      (lambda (output)
                        (let ((uuid-len 36))
                          (substring output 0 uuid-len)))
                      ($andp '$uuidgen-p))
                     "uuid"))
        ((require 'uuidgen nil t)
         (uuidgen-4))))

(defun $uuidgen-p (s)
  "Is S an ID created by UUIDGEN?"
  (string-match "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'" (downcase s)))

(defun $buffer-file-name (&optional name)
  ;; The following code is borrowed from `lisp/files.el' in Emacs source code.
  (setq name (or name buffer-file-name))
  (and name
       (let ((remote-id (file-remote-p name)))
         ;; Remove backup-suffixes from file name.
         (setq name (file-name-sans-versions name))
         ;; Remove remote file name identification.
         (when (and (stringp remote-id)
                    (string-match (regexp-quote remote-id) name))
           (setq name (substring name (match-end 0))))
         name)))

(defun $file-name-match (regexp name)
  ;; The following code is borrowed from `lisp/files.el' in Emacs source code.
  (when name
    (setq name ($buffer-file-name name))
    (if (memq system-type '(windows-nt cygwin))
        ;; System is case-insensitive.
        (let ((case-fold-search t))
          (string-match regexp name))
      ;; System is case-sensitive.
      (or
       ;; First match case-sensitively.
       (let ((case-fold-search nil))
         (string-match regexp name))
       ;; Fallback to case-insensitive match.
       (and auto-mode-case-fold
            (let ((case-fold-search t))
              (string-match regexp name)))))))

;; This code is borrowed from:
;;
;; - [[https://stackoverflow.com/a/9059906]]
;; - [[https://github.com/mariusk/android-with-emacs/blob/fb65f49666766e8c25b23b0377d086f6e55a3f5b/README.md]]
(defun $get-closest-pathname (file &optional max-level)
  (let* ((root (expand-file-name "/"))
         (level 0)
         (dir (cl-loop
               for d = default-directory then (expand-file-name ".." d)
               do (setq level (+ level 1))
               if (file-exists-p (expand-file-name file d))
               return d
               if (and max-level (> level max-level))
               return nil
               if (equal d root)
               return nil)))
    (when dir
      (expand-file-name file dir))))


;;; Avoid killing emacs by mistake
(setq confirm-kill-emacs 'yes-or-no-p)


;;; System Environment
;; Git cannot detect if it's run in Emacs.
(setenv "GIT_PAGER" "")


;;; Set fill column to 79
;; A line begins at column 0 in Emacs.  79 is the last column on an 80-width
;; screen.  Do not occupy column 79, leave it for filling.
;;
;; This setting will make Emacs fill the following paragraphs like this:
;;
;; ----------------------------------------------------------------------------

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;; ----------------------------------------------------------------------------

(setq-default fill-column 79)


;;; Disable `indent-tabs-mode'.
;; `web-mode' changes indentation settings if `indent-tabs-mode' is non-nil.
;; To prevent it, set the default value of `indent-tabs-mode' to `nil'.
(setq-default indent-tabs-mode nil)


;;; Cut & Paste, Kill Ring, Clipboard, Selection
;;; [[info:emacs#Cut%20and%20Paste]]
(setq select-enable-primary t
      select-enable-clipboard t)


;;; Don't add the final newline globally.
;;;
;;; Use editorconfig to force Emacs to add the final newline in certain files.
;;;
;;; [[info:emacs#Customize%20Save]]
(setq require-final-newline nil
      mode-require-final-newline nil)


;;; Don't delete trailing lines when calling `delete-trailing-whitespace' on
;;; the entire buffer.
(setq delete-trailing-lines nil)


;;; Disable backup when saving.
;;; [[info:emacs#Backup]]
(setq make-backup-files nil)
(setq version-control nil)


;;; Keep point at the same screen position after scrolling.
;;; [[info:emacs#Scrolling]]
(setq scroll-preserve-screen-position t)


;;; UI Configuration
(setq-default cursor-in-non-selected-windows nil
              indicate-buffer-boundaries 'left
              indicate-empty-lines t)

(setq mouse-yank-at-point t
      display-time-24hr-format t
      display-time-day-and-date t
      visible-bell t
      inhibit-startup-screen t)


;;; Theme
(defvar **theme-engine**
  (cond ((>= emacs-major-version 24) 'custom-theme)
        ((>= emacs-major-version 21) 'color-theme)))

(setq custom-theme-directory (locate-user-emacs-file "themes/"))

(defvar **custom-themes** '())

(defvar **color-themes** '())

(defun $theme-list ()
  (cond ((eq **theme-engine** 'custom-theme)
         **custom-themes**)
        ((eq **theme-engine** 'color-theme)
         **color-themes**)
        (t '())))

(defvar **theme-initialized** nil)

(defun $theme-initialize ()
  (unless **theme-initialized**
    (cond ((eq **theme-engine** 'custom-theme)
           ;; `custom-theme' has been initialized during startup (See
           ;; `lisp/loadup.el`).
           (setq **theme-initialized** t))
          ((eq **theme-engine** 'color-theme)
           (when (require 'color-theme nil t)
             ;; The docstring of `color-theme-is-cumulative' is wrong.
             ;;
             ;; Installing a new theme will undo all settings only if *nil*.
             ;;
             ;; http://www.emacswiki.org/emacs/ColorThemeQuestions#toc9
             ;; http://savannah.nongnu.org/bugs/?29500
             (setq color-theme-is-cumulative nil)
             ;; The variable `color-theme-initialized' is not used in
             ;; `color-theme'.  There is no way to determine whether
             ;; `color-theme` has been initialized.
             (setq **theme-initialized** t))))))

(defun $random-elt (sequence)
  (nth (random (length sequence)) sequence))

(defvar **theme-selector** '$random-elt)

(defun $select-theme ()
  (funcall **theme-selector** ($theme-list)))

(defvar **theme** nil)

;; **TODO** If emacs is running in daemon mode, setting a theme in a frame will
;; cause the font in current frame become very small.  We have to create a new
;; frame and close the old one.
;;
;; If there have been already 2 frames before setting theme, the font in
;; current frame will not change.
(defun $set-theme (&optional theme)
  (interactive
   (list
    (let ((input (completing-read
                  "Theme (leave blank or `nil' to select one randomly): "
                  (mapcar 'symbol-name
                          (delete-dups
                           (append
                            ($theme-list)
                            (cond ((eq **theme-engine** 'custom-theme)
                                   (custom-available-themes))
                                  ((eq **theme-engine** 'color-theme)
                                   color-themes)
                                  (t '()))
                            '()))))))
      (if (equal "" input) nil (intern input)))))
  (when (and **theme-initialized**
             (setq theme (or theme ($select-theme))))
    (message "Theme: %s" theme)
    (cond ((eq **theme-engine** 'custom-theme)
           (mapc 'disable-theme (copy-sequence custom-enabled-themes))
           (when (load-theme theme t)
             (setq **theme** theme)))
          ((and (eq **theme-engine** 'color-theme)
                (featurep 'color-theme))
           ;; The first theme should not undo the original settings.
           (let ((color-theme-is-cumulative (null **theme**)))
             (funcall theme))
           (setq **theme** theme)))))


;;; Font Configuration
(defun $set-font (&optional frame)
  (interactive)
  (unless frame
    (setq frame (selected-frame)))
  (when (display-graphic-p frame)
    (unless (featurep 'cnfonts)
      ;; If `cnfonts' is not installed, run the following setup.
      ;;
      ;; `face-font-rescale-alist' has to be set when setting the font
      ;; configuration.
      ;;
      ;; Setting it during loading `.emacs' raises the following error if Emacs
      ;; is running in daemon mode or with `-nw' option:
      ;;
      ;;     set-face-attribute: Font not available: #<font-spec nil nil nil nil nil nil nil nil nil nil nil nil nil>
      ;;
      (add-to-list 'face-font-rescale-alist '("-Droid Sans Fallback-" . 1.25))
      (add-to-list 'face-font-rescale-alist '("-WenQuanYi Micro Hei Mono-" . 1.25))
      (add-to-list 'face-font-rescale-alist '("-文泉驿等宽微米黑-" . 1.25))
      (set-frame-font "DejaVu Sans Mono-11" t (list frame))
      (dolist (script '(han kana hangul symbol cjk-misc bopomofo))
        ;; There're no hangul characters in `fonts-droid` on Debian.  Use
        ;; `ttf-wqy-microhei` instead.
        (set-fontset-font nil script "WenQuanYi Micro Hei Mono-11" frame)))))


;;; Font width test
;;;
;;; 01234567890123456789012345678901234567890123456789012345678901234567890123456789
;;; 零一二三四五六七八九零一二三四五六七八九零一二三四五六七八九零一二三四五六七八九


;;; function to be executed for every new frame
(defun $after-make-frame-functions (frame)
  ;; Introduce it again, because setting font in non-GUI frame will raise
  ;; error.
  ($set-font frame))

(add-hook 'after-make-frame-functions '$after-make-frame-functions t)


;;; Garbage ls/gcc output
;;; [[http://www.emacswiki.org/emacs/AnsiColor]]
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;;; Comparing Files in Unified Format
;;; [[info:emacs#Comparing%20Files]]
(setq diff-switches "-u")


;;; Use `fill-column' for `comment-indent'.
(setq comment-fill-column nil)


;;; Prog Mode
(defun $prog-mode-hook ()
  (when (require 'linum nil 'noerror)
    (linum-mode 1))
  ;; Enable Whitespace mode.
  (when (require 'whitespace nil 'noerror)
    (whitespace-mode 1))
  ;; Disable Fill-Column-Indicator mode.
  ;;
  ;; fci-mode has many issues [1].  DO NOT enable it.
  ;;
  ;; [1] https://github.com/alpaker/Fill-Column-Indicator/issues
  (when (require 'fci-mode nil 'noerror)
    (fci-mode -1))
  ;; Enable Rainbow-Delimiters mode.
  (when (require 'rainbow-delimiters nil 'noerror)
    (rainbow-delimiters-mode))
  (when (require 'flycheck nil 'noerror)
    (flycheck-mode 1))
  ;; Enable Flyspell Prog Mode.  This invokes `(flyspell-mode 1)'.
  ;; Eval `(flyspell-mode -1)' to disable it.
  (when (require 'flyspell nil 'noerror)
    (flyspell-prog-mode))
  (when (require 'indent-guide nil 'noerror)
    (indent-guide-mode 1))
  (when (require 'aggressive-indent nil 'noerror)
    (aggressive-indent-mode 1))
  (when (require 'pangu-spacing nil 'noerror)
    (pangu-spacing-mode 1))
  ;; Do not insert tabs in indentation by default.
  ;;
  ;; NOTE: Setting `indent-tabs-mode' to `t' does NOT mean "pressing `TAB'
  ;; inserts a `\t'".
  (setq indent-tabs-mode nil))

(add-hook 'prog-mode-hook '$prog-mode-hook)

(defun $run-prog-mode-hook ()
  "Put this function into a hook of any programming related mode,
to ensure that `prog-mode-hook' could be executed even if the
major mode isn't derived from `prog-mode'."
  (unless (derived-mode-p 'prog-mode)
    (run-mode-hooks 'prog-mode-hook)))


;;; CamelCase Mode
(define-minor-mode $camel-case-mode
  "It just combines `subword-mode' and `glasses-mode'."
  :init-value nil
  (let ((arg (if $camel-case-mode 1 -1)))
    (subword-mode arg)))


;;; Lisp Common Mode
(defun $lisp-mode-common-hook ()
  ($run-prog-mode-hook))


(load (locate-user-emacs-file "bootstrap"))


(define-advice update-directory-autoloads (:around (fn &rest r) dont-update-time-stamp-and-copyright)
  "This functions runs `before-save-hook'.  Since updating autoloads is a
background operation, we must skip the hooks which modify the file and keep the
file unmodified.

The call stack:

`package-install'
-> `package-download-transaction'
-> `package-install-from-archive'
-> `package-unpack'
-> `package--make-autoloads-and-stuff'
-> `package-generate-autoloads'
-> `update-directory-autoloads'
-> `save-buffer'
-> `basic-save-buffer'
-> `before-save-hook'
"
  (let ((time-stamp-active nil)
        (copyright-update nil))
    (apply fn r)))


;;; Some packages have multiple variants, such as `org' and `org-plus-contrib'.
;;; We have to ensure that emacs will always install one of the variants,
;;; because some packages, e.g. `org-edna', put another variant in its
;;; dependency.
;;;
;;; https://emacs.stackexchange.com/a/26513
(define-advice package-compute-transaction (:filter-return (packages) map)
  (let ((maps '((org . org-plus-contrib))))
    (dolist (map maps)
      (let* ((from (car map))
             (to (cdr map))
             (to-pkg (car (cdr (assq to package-archive-contents)))))
        (setq packages (seq-remove (lambda (pkg)
                                     (eq (package-desc-name pkg) from))
                                   packages))
        (unless (or (package-installed-p to)
                    (seq-find (lambda (pkg)
                                (eq (package-desc-name pkg) to))
                              packages))
          (setq packages (cons to-pkg packages)))))
    packages))


(setq use-package-always-defer t)
(setq use-package-always-ensure t)
;; Install packages after `(req-package-finish)'.
;;
;; https://github.com/edvorg/req-package/issues/51#issuecomment-362154387
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package nil 'noerror)


(use-package quelpa
  :demand t
  :ensure t
  :init
  (setq quelpa-upgrade-p nil)
  ;; If `quelpa-upgrade-p' is `t', Emacs tries to upgrade quelpa packages when
  ;; the `use-package' macro is evaluated.  This causes Emacs connects to
  ;; remote sites every time it starts.  We can't wait for it.  Upgrade quelpa
  ;; packages manually please.
  (setq quelpa-update-melpa-p nil))

(define-advice quelpa-build--build-single-file-package (:around (fn &rest r) dont-update-time-stamp-and-copyright)
  "This function runs `before-save-hook'.  Since installing package is a
background operation, we must skip the hooks which modify the file and keep the
file unmodified.

The call stack:

`quelpa'
-> `quelpa-build--build-single-file-package'
-> `write-file'
-> `save-buffer'
-> `basic-save-buffer'
-> `before-save-hook'
"
  (let ((time-stamp-active nil)
        (copyright-update nil))
    (apply fn r)))


(use-package quelpa-use-package
  :demand t
  :ensure t)


(use-package req-package
  :demand t
  :ensure t)


(use-package load-dir
  :demand t
  :ensure t)


(let ((load-dirs (locate-user-emacs-file "init.d")))
  (load-dirs))


(setq **custom-themes**
      (delete-dups
       (append **custom-themes**
               ;; built-in
               '(wombat tsdh-dark tango-dark manoj-dark deeper-blue)
               '())))

(setq **color-themes**
      (delete-dups
       (append **color-themes**
               ;; built-in
               '(color-theme-tty-dark
                 color-theme-dark-laptop
                 color-theme-hober
                 color-theme-midnight)
               '())))


;; The following content is always inserted by `package--ensure-init-file',
;; which is called by `package-initialize'.  But we comment the s-exp out here,
;; let Emacs call `package-initialize' after loading `user-init-file'.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)


(load "~/.emacs_local.el" t)


(defun $after-init-hook ()
  (req-package-finish)
  (auto-package-update-maybe)
  ;; Special Mode
  ;;
  ;; Emacs has deleted `z' binding in GIT#0d4505d & GIT#82dffff .  We
  ;; restore it here.
  (unless (lookup-key special-mode-map "z")
    (define-key special-mode-map "z" 'kill-this-buffer))
  ;; Scroll Lock Mode
  (global-set-key [Scroll_Lock] 'scroll-lock-mode)
  ;; Midnight Mode
  ;; [[info:emacs#Kill%20Buffer]]
  (require 'midnight nil t)
  ;; Auto Revert Mode
  (global-auto-revert-mode 1)
  (tool-bar-mode -1)
  (set-scroll-bar-mode 'left)
  (blink-cursor-mode -1)
  (mouse-avoidance-mode 'exile)
  (column-number-mode 1)
  (display-time)
  ;; Which Function Mode
  (which-function-mode 1)
  ;; Enable Font-Lock Mode Globally.
  (global-font-lock-mode 1)
  ;; Highlight matching parenthesis
  (show-paren-mode 1)
  (global-undo-tree-mode 1)
  (global-fringe-current-line-mode 1)
  (which-key-mode 1)
  (which-key-setup-side-window-right)
  (when (require 'auth-source-pass nil 'noerror)
    (auth-source-pass-enable))
  (focus-autosave-mode 1)
  (super-save-mode 1)
  (smart-mark-mode 1)
  (editorconfig-mode 1)
  (df-mode 1)
  (when (require 'cnfonts nil 'noerror)
    (cnfonts-enable))
  (ido-mode 1)
  (require 'uniquify)
  (require 'generic-x)
  ;; Load MMM Mode autoloads & default settings
  (require 'mmm-auto)
  (require 'mmm-defaults)
  ;; Use X Window to prevent RSI.
  (type-break-mode -1)
  ;; Disable by default since it doesn't work well in some modes such as
  ;; `yaml-mode'.
  (electric-indent-mode -1)
  (global-homebrew-mode 1)
  (beginend-global-mode 1)
  (auto-insert-mode 1)
  (global-company-mode 1)
  (projectile-mode 1)
  (wrap-region-global-mode 1)
  (smartparens-global-strict-mode 1)
  (show-smartparens-global-mode 1)
  (exec-path-from-shell-initialize)
  ;; IMPORTANT: Define `flyspell-delayed-commands' before loading `dashboard'.
  ;;
  ;; `dashboard' loads `org'.  `org' will modify `flyspell-delayed-commands'
  ;; after `flyspell' loaded.  Unfortunately we have an init file called
  ;; `flyspell.el'.  It triggers the `eval-after-load' code, but at that moment
  ;; `flyspell-delayed-commands' is undefined.  This will cause an error.
  ;;
  ;; To solve this issue, we define `flyspell-delayed-commands' *right before*
  ;; loading `dashboard'.
  (defvar flyspell-delayed-commands '())
  (require 'dashboard)
  (when (string= server-name "systemd")
    (atomic-chrome-start-server))
  (unless (bound-and-true-p **org-timer**)
    (setq **org-timer** (run-at-time nil 3600 'org-agenda-to-appt)))
  (appt-activate 1)
  (unless **theme-initialized**
    ($theme-initialize))
  ($set-theme)
  (unless (daemonp)
    (server-start)
    ;; Setup initial frame if emacs isn't running as daemon.
    ;;
    ;; We have to run `after-make-frame-functions' in `run-at-time'.  Setting
    ;; font directly doesn't take effect.  I don't know why.
    ;;
    ;; Pass `(selected-frame)' as argument to `after-make-frame-functions'
    ;; because the local variable bindings made by Emacs Lisp are dynamic
    ;; binding, by default.  We must pass the initial frame.
    (run-at-time 1 nil 'run-hook-with-args 'after-make-frame-functions (selected-frame))))

;; `$after-init-hook' should be added at the end because it should be
;; run after `color-theme-backup-original-values'.
(add-hook 'after-init-hook '$after-init-hook t)
