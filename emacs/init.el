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
                             '("themes/")))))


;;; Key bindings
;; [[http://www.emacswiki.org/emacs/PrefixKey]]
(define-prefix-command '$extended-map)
(global-set-key "\C-cx" '$extended-map)


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
;;; ** What is Message Mode?
;;;
;;;    Message mode is an alternative to mail mode for composing and
;;;    sending messages inside emacs.  It's the preferred mode used by
;;;    gnus.  However it can be used independently from gnus.
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
      mail-user-agent 'message-user-agent
      ;; Set domain part of Message-Ids to a fully qualified domain name.
      ;; [[info:message#News%20Headers]]
      message-user-fqdn (or (bound-and-true-p message-user-fqdn)
                            (let ((parts (split-string user-mail-address "@")))
                              (and (> (length parts) 1) (last parts)))
                            "mail.gmail.com")
      ;; `message-from-style' overrides `mail-from-style' in message mode.
      ;; [[info:message#Message%20Headers]]
      ;; [[info:message#News%20Headers]]
      message-from-style 'angles
      ;; Turn off auto-fill-mode, but filling can be done by manual.
      message-fill-column nil
      ;; Do not use `unsent'.  `C-u C-x m' cannot switch to such a
      ;; buffer whose name begins with "*unsent ".
      message-generate-new-buffers 'unique
      ;; Ask for confirmation when sending a message.
      message-confirm-send t
      ;; Kill the message buffer after sending a message.
      message-kill-buffer-on-exit t)

(defun $message-setup-hook ()
  (flyspell-mode 1))

(add-hook 'message-setup-hook '$message-setup-hook)

(defun $trim (s)
  "Remove whitespace at beginning and end of string."
  (if (string-match "\\`[ \t\n\r]+" s) (setq s (replace-match "" t t s)))
  (if (string-match "[ \t\n\r]+\\'" s) (setq s (replace-match "" t t s)))
  s)

(defun $uuid ()
  "Return an UUID."
  (cond ((let ((uuid (executable-find "uuidgen")))
           (and uuid ($trim (shell-command-to-string uuid)))))
        ((require 'uuidgen nil t)
         (uuidgen-4))))

(defun $uuidgen-p (s)
  "Is S an ID created by UUIDGEN?"
  (string-match "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'" (downcase s)))

(defun $message-unique-id-by-uuid (unique-id)
  "Return an UUID if available.  Otherwise, return the original
return value of `message-unique-id'."
  (let ((uuid ($uuid)))
    (if ($uuidgen-p uuid)
        uuid
      unique-id)))

(advice-add 'message-unique-id :filter-return '$message-unique-id-by-uuid)

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


;;; Special Mode
;;;
;;; Emacs has deleted `z' binding in GIT#0d4505d & GIT#82dffff .  We
;;; restore it here.
(unless (lookup-key special-mode-map "z")
  (define-key special-mode-map "z" 'kill-this-buffer))


;;; Text Mode
(defun $text-mode-hook ()
  (flyspell-mode 1))

(add-hook 'text-mode-hook '$text-mode-hook)


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


;;; Scroll Lock Mode
(global-set-key [Scroll_Lock] 'scroll-lock-mode)


;;; View Mode
;; Enter View mode when a buffer become read-only.
(setq view-read-only t)

(defun $eval-after-load-view ()
  ;; Bind "less"-like key bindings
  (define-key view-mode-map "N" 'View-search-last-regexp-backward)
  (define-key view-mode-map "?" 'View-search-regexp-backward)
  (define-key view-mode-map "G" 'View-goto-line-last)
  (define-key view-mode-map "k" 'View-scroll-line-backward)
  (define-key view-mode-map "j" 'View-scroll-line-forward)
  (define-key view-mode-map "b" 'View-scroll-page-backward)
  (define-key view-mode-map "f" 'View-scroll-page-forward))

(eval-after-load 'view '($eval-after-load-view))


;;; Midnight Mode
;;; [[info:emacs#Kill%20Buffer]]
(require 'midnight nil t)


;;; Ido Mode
;; [[http://www.emacswiki.org/emacs/InteractivelyDoThings]]
;; [[http://ergoemacs.org/emacs/emacs_icomplete_vs_ido.html]]
;; [[http://ergoemacs.org/emacs/emacs_iswitch_vs_ido.html]]
(setq ido-confirm-unique-completion t)
;; Disable automatic file search in ido mode
;;
;; [[http://stackoverflow.com/a/18089076]]
(setq ido-auto-merge-work-directories-length -1)
(ido-mode 1)


;;; IbufferMode
;;; [[http://www.emacswiki.org/emacs/IbufferMode]]
;;
;; * Ibuffer hooks
;;
;; ** `ibuffer-load-hook'
;;
;;    run once after ibuffer loaded.
;;
;; ** `ibuffer-mode-hook'
;;
;;    run when a buffer goes into `ibuffer-mode'.
;;
;; ** `ibuffer-hook'
;;
;;    run when executing `ibuffer' even if "*Ibuffer*" exists.
(global-set-key "\C-x\C-b" 'ibuffer)

;; `ibuffer-auto-mode' should run in `ibuffer-mode-hook' because if
;; should run only once when the buffer created.
(defun $ibuffer-mode-hook ()
  (ibuffer-auto-mode 1))

(add-hook 'ibuffer-mode-hook '$ibuffer-mode-hook)


;;; Make Buffer Names Unique
;;; [[info:emacs#Uniquify]]
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward))


;;; Help Mode
(defun $help-mode-hook ()
  (scroll-lock-mode 1))

(add-hook 'help-mode-hook '$help-mode-hook)


;;; Cut & Paste, Kill Ring, Clipboard, Selection
;;; [[info:emacs#Cut%20and%20Paste]]
(setq x-select-enable-primary t
      x-select-enable-clipboard t)


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


;;; Checking and Correcting Spelling
;; Use aspell for spell-checking.
(setq ispell-program-name "aspell")

;; Flyspell
(setq flyspell-use-meta-tab nil)


;;; Auto Revert Mode
(global-auto-revert-mode 1)


;;; UI Configuration
(setq-default cursor-in-non-selected-windows nil
              indicate-buffer-boundaries 'left
              indicate-empty-lines t)

(setq mouse-yank-at-point t
      display-time-24hr-format t
      display-time-day-and-date t
      visible-bell t
      inhibit-startup-screen t)

(tool-bar-mode -1)
(set-scroll-bar-mode 'left)
(blink-cursor-mode -1)
(mouse-avoidance-mode 'exile)
(column-number-mode 1)
(display-time)


;;; Theme
(defvar **theme-engine**
  (cond ((>= emacs-major-version 24) 'custom-theme)
        ((>= emacs-major-version 21) 'color-theme)))

(defvar **custom-themes** '())

(defvar **color-themes** '())

(defun $theme-list ()
  (cond ((eq **theme-engine** 'custom-theme)
         **custom-themes**)
        ((eq **theme-engine** 'color-theme)
         **color-themes**)
        (t '())))

;; color-theme
(defun $eval-after-load-color-theme ()
  (color-theme-initialize)
  ;; NIL doesn't work any more.
  (setq color-theme-is-global t))

(eval-after-load 'color-theme '($eval-after-load-color-theme))

;; color-theme-blackboard
(autoload 'color-theme-blackboard "color-theme-blackboard" nil t)
(add-to-list '**color-themes** 'color-theme-blackboard)

;; color-theme-hober2
(autoload 'color-theme-hober2 "color-theme-hober2" nil t)
(add-to-list '**color-themes** 'color-theme-hober2)

;; hober2-theme
(add-to-list '**custom-themes** 'hober2)

;; color-theme-tango
(autoload 'color-theme-tango "color-theme-tango" nil t)
(add-to-list '**color-themes** 'color-theme-tango)

;; color-theme-tangotango
(add-to-list '**custom-themes** 'tangotango)
(autoload 'color-theme-tangotango "color-theme-tangotango" nil t)
(add-to-list '**color-themes** 'color-theme-tangotango)

;; color-theme-empty-void
(autoload 'color-theme-empty-void "color-theme-empty-void" nil t)
(add-to-list '**color-themes** 'color-theme-empty-void)

;; color-theme-inkpot
(autoload 'color-theme-inkpot "color-theme-inkpot" nil t)
(add-to-list '**color-themes** 'color-theme-inkpot)

;; color-theme-wombat
(autoload 'color-theme-wombat "color-theme-wombat" nil t)
(add-to-list '**color-themes** 'color-theme-wombat)

;; color-theme-wombat-dark
(autoload 'color-theme-wombat-dark "color-theme-wombat-dark" nil t)
(add-to-list '**color-themes** 'color-theme-wombat-dark)

;; zenburn-theme
(add-to-list 'safe-local-eval-forms
             '(rainbow-mode 1))
(add-to-list '**custom-themes** 'zenburn)

;; color-theme-zenburn
(add-to-list '**color-themes** 'color-theme-zenburn)

(setq **custom-themes**
      (delete-dups
       (append **custom-themes**
               ;; color-theme-modern
               '(tty-dark
                 arjen
                 billw
                 calm-forest
                 clarity
                 classic
                 dark-blue2
                 dark-laptop
                 deep-blue
                 desert
                 euphoria
                 gnome2
                 goldenrod
                 gray30
                 hober
                 jonadabian-slate
                 jonadabian
                 kingsajz
                 late-night
                 lawrence
                 ld-dark
                 midnight
                 oswald
                 pok-wob
                 pok-wog
                 raspopovic
                 renegade
                 resolve
                 retro-orange
                 robin-hood
                 ryerson
                 shaman
                 simple-1
                 sitaramv-solaris
                 subtle-hacker
                 taming-mr-arneson
                 taylor
                 word-perfect
                 subdued)
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
    (unless (featurep 'chinese-fonts-setup)
      ;; If `chinese-fonts-setup' is not installed, run the following setup.
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


;;; Browse URL
(autoload 'browse-url-interactive-arg "browse-url")

(defun $browse-url-conkeror (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-generic-program "conkeror")
        (browse-url-generic-args nil))
    (browse-url-generic url new-window)))

(defun $browse-url-chromium (url &optional new-window)
  "Ask the Chromium WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-chromium-arguments' are also passed to
Chromium."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "chromium " url) nil
           (or (bound-and-true-p browse-url-chromium-program) "chromium")
           (append
            (bound-and-true-p browse-url-chromium-arguments)
            (list url)))))

(defun $browse-url-chromium/incognito (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-chromium-arguments
         (cons "--incognito" (bound-and-true-p browse-url-chromium-arguments))))
    (cond ((<= emacs-major-version 23)
           ($browse-url-chromium url new-window))
          (t
           (browse-url-chromium url new-window)))))

(defun $browse-url-default-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-generic-program
         (or (executable-find "xdg-open")
             (executable-find "sensible-browser"))))
    (browse-url-generic url new-window)))

(setq browse-url-browser-function '$browse-url-default-browser)


;;; MPC
(setq mpc-browser-tags '(Artist|Composer|Performer Album|Playlist Title))


;;; Comparing Files in Unified Format
;;; [[info:emacs#Comparing%20Files]]
(setq diff-switches "-u")


;;; Which Function Mode
(which-function-mode 1)


;;; Enable Font-Lock Mode Globally.
(global-font-lock-mode 1)


;;; Highlight matching parenthesis
(show-paren-mode 1)


;;; Use `fill-column' for `comment-indent'.
(setq comment-fill-column nil)


;;; Prog Mode
(defun $prog-mode-hook ()
  (linum-mode 1)
  ;; Enable Whitespace mode.
  (whitespace-mode 1)
  ;; Disable Fill-Column-Indicator mode.
  ;;
  ;; fci-mode has many issues [1].  DO NOT enable it.
  ;;
  ;; [1] https://github.com/alpaker/Fill-Column-Indicator/issues
  (fci-mode -1)
  ;; Enable Rainbow-Delimiters mode.
  (rainbow-delimiters-mode)
  (flycheck-mode 1)
  ;; Enable Flyspell Prog Mode.  This invokes `(flyspell-mode 1)'.
  ;; Eval `(flyspell-mode -1)' to disable it.
  (flyspell-prog-mode)
  (indent-guide-mode 1)
  (when (and nil (require 'cedit nil t))
    ;; Strings cannot contain non-ASCII control characters.  Use `(kbd "C-.")'
    ;; or `[?\C-.]' etc instead.
    ;;
    ;; [[https://stackoverflow.com/a/2483459]]
    ;; [[https://stackoverflow.com/a/10088297]]
    ;; [[https://lists.gnu.org/archive/html/help-gnu-emacs/2007-08/msg00397.html]]
    (local-set-key (kbd "C-f") 'cedit-forward-char)
    (local-set-key (kbd "C-b") 'cedit-backward-char)
    (local-set-key (kbd "M-a") 'cedit-beginning-of-statement)
    (local-set-key (kbd "M-e") 'cedit-end-of-statement)
    (local-set-key (kbd "C-M-d") 'cedit-down-block)
    (local-set-key (kbd "C-M-u") 'cedit-up-block-backward)
    (local-set-key (kbd "C-)") 'cedit-slurp)
    (local-set-key (kbd "C-<right>") 'cedit-slurp)
    (local-set-key (kbd "M-(") 'cedit-wrap-brace)
    (local-set-key (kbd "C-}") 'cedit-barf)
    (local-set-key (kbd "C-<left>") 'cedit-barf)
    (local-set-key (kbd "M-<up>") 'cedit-splice-killing-backward)
    (local-set-key (kbd "M-r") 'cedit-raise))
  ;; Do not insert tabs in indentation by default.
  ;;
  ;; NOTE: Setting `indent-tabs-mode' to `t' does NOT mean "pressing `TAB'
  ;; inserts a `\t'".
  (setq indent-tabs-mode nil))

(add-hook 'prog-mode-hook '$prog-mode-hook)

(defun $prog-mode-hook* ()
  "Put this function into a hook of any programming related mode,
to ensure that `$prog-mode-hook' could be executed even if the
major mode isn't derived from `prog-mode'."
  (unless (derived-mode-p 'prog-mode)
    ($prog-mode-hook)))


;;; CamelCase Mode
(define-minor-mode $camel-case-mode
  "It just combines `subword-mode' and `glasses-mode'."
  :init-value nil
  (let ((arg (if $camel-case-mode 1 -1)))
    (subword-mode arg)))


;;; Lisp Common Mode
(defun $lisp-mode-common-hook ()
  (enable-paredit-mode)
  (when (and nil (require 'cedit nil t))
    (local-set-key (kbd "C-)") 'cedit-or-paredit-slurp)
    (local-set-key (kbd "C-<right>") 'cedit-or-paredit-slurp)
    (local-set-key (kbd "C-}") 'cedit-or-paredit-barf)
    (local-set-key (kbd "C-<left>") 'cedit-or-paredit-barf)
    (local-set-key (kbd "M-<up>") 'cedit-or-paredit-splice-killing-backward)
    (local-set-key (kbd "M-r") 'cedit-or-paredit-raise)))


(load (locate-user-emacs-file "bootstrap"))


(setq use-package-always-defer t)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package nil 'noerror)


(use-package req-package
  :demand t
  :ensure t)


(use-package load-dir
  :ensure t)


(let ((load-dirs (locate-user-emacs-file "init.d")))
  (load-dirs))


(load "~/.emacs_local.el" t)


(defun $after-init-hook ()
  (req-package-finish)
  (auto-package-update-maybe)
  (global-undo-tree-mode 1)
  (global-fringe-current-line-mode 1)
  (which-key-mode 1)
  (which-key-setup-side-window-right)
  (auth-pass-enable)
  (focus-autosave-mode 1)
  (super-save-mode 1)
  (smart-mark-mode 1)
  (editorconfig-mode 1)
  (df-mode 1)
  (require 'chinese-fonts-setup nil 'noerror)
  (global-pointback-mode 1)
  (require 'generic-x)
  ;; Load MMM Mode autoloads & default settings
  (require 'mmm-auto)
  (require 'mmm-defaults)
  ;; Use X Window to prevent RSI.
  (type-break-mode -1)
  ;; Disable by default since it doesn't work well in some modes such as
  ;; `yaml-mode'.
  (electric-indent-mode -1)
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
