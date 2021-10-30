;; Require `cl-lib'.
;;
;; Do not require `cl`.  It defines functions without the 'cl-' prefix, that
;; pollutes the namespace.
;;
;; - [[info:cl#Usage]]
;; - [[info:cl#Organization]]
(require 'cl-lib nil t)

(random t)

(defun $skipp (abs-file)
  (member (file-name-directory abs-file)
          (list
           (file-name-as-directory
            (expand-file-name user-emacs-directory))
           (file-name-as-directory
            (expand-file-name "init.d" user-emacs-directory)))))

(define-advice do-after-load-evaluation (:around (fn abs-file) skip)
  "Do not run `eval-after-load' code for some files.

Why do I need this?  Some config files may have the same names as library
files.  When a config file is loaded, this function will trigger some code in
`after-load-alist'.  But at that time the library file has not been loaded.  It
will cause errors since some variables or functions have not been defined.

Case 1: org and flyspell

org will insert a function into `flyspell-delayed-commands' after `flyspell'
loaded.

Case 2: sendmail

Some functions, such as `magit-status' and `list-packages', will require
`mailabbrev', which defines key bindings in `mail-mode-map' after `sendmail'
loaded.

It's safe to just skip such files."
  (unless (and (stringp abs-file)
               ($skipp abs-file))
    (funcall fn abs-file)))

(define-advice load-history-filename-element (:override (file-regexp) skip)
  "Do not run `eval-after-load' code for some files.

If the file has been loaded.  `eval-after-load' will execute the code
immediately.  Do not return unwanted file from `load-history'."
  (let* ((loads load-history)
         (load-elt (and loads (car loads))))
    (save-match-data
      (while (and loads
                  (or (null (car load-elt))
                      (not (string-match file-regexp (car load-elt)))
                      ($skipp (car load-elt))))
        (setq loads (cdr loads)
              load-elt (and loads (car loads)))))
    load-elt))

(defvar package-alist)
(declare-function package-delete "ext:package")
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

;; (setq load-path
;;       (apply 'append
;;              **default-load-path**
;;              (mapcar '$subdirs-to-list
;;                      (mapcar 'locate-user-emacs-file
;;                              '("site-lisp/")))))

;; (setq custom-theme-load-path
;;       (apply 'append
;;              **default-custom-theme-load-path**
;;              (mapcar '$subdirs-to-list
;;                      (mapcar 'locate-user-emacs-file
;;                              '()))))


;;; Specify custom file, but not load it.
;;; [[info:emacs#Saving%20Customizations]]
(setq custom-file (locate-user-emacs-file ".emacs-custom.el"))
;; As init file may use write customization to `custom-file', we have to set
;; this variable at the beginning.


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

(declare-function uuidgen-4 "ext:uuidgen")
(defun $uuid ()
  "Return an UUID."
  (or (seq-reduce '$funcall-when
                  (list
                   ($andp 'file-readable-p)
                   (lambda (uuid-file)
                     (with-temp-buffer
                       (let ((uuid-len 36))
                         (and (<= uuid-len (nth 1 (insert-file-contents uuid-file)))
                              (buffer-substring (point-min) (+ (point-min) uuid-len))))))
                   ($andp '$uuidgen-p))
                  "/proc/sys/kernel/random/uuid")
      (seq-reduce '$funcall-when
                  (list
                   'executable-find
                   'shell-command-to-string
                   (lambda (output)
                     (let ((uuid-len 36))
                       (substring output 0 uuid-len)))
                   ($andp '$uuidgen-p))
                  "uuidgen")
      (and (require 'uuidgen nil t)
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


;;; System Environment
;; Git cannot detect if it's run in Emacs.
(setenv "GIT_PAGER" "")


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
(defvar package-archive-contents)
(declare-function package-desc-name "ext:package")
(declare-function package-installed-p "ext:package")
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


(mapc 'load
      (mapcar 'locate-user-emacs-file
              '("bootstrap"
                "use-package"
                "req-package"
                "load-dir"
                "theme")))


(defvar **custom-themes**)
(setq **custom-themes**
      (delete-dups
       (append **custom-themes**
               ;; built-in
               '((wombat) (tsdh-dark) (tango-dark) (manoj-dark) (deeper-blue))
               '())))

(defvar **color-themes**)
(setq **color-themes**
      (delete-dups
       (append **color-themes**
               ;; built-in
               '((color-theme-tty-dark . (lambda () t))
                 (color-theme-dark-laptop)
                 (color-theme-hober)
                 (color-theme-midnight))
               '())))


;; The following content is always inserted by `package--ensure-init-file',
;; which is called by `package-initialize'.  But we comment the s-exp out here,
;; let Emacs call `package-initialize' after loading `user-init-file'.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)


(declare-function req-package-finish "ext:req-package")
(declare-function el-get "ext:el-get")
(declare-function dashboard-setup-startup-hook "ext:dashboard")
(defun $after-init-hook ()
  (req-package-finish)
  (el-get)
  ;; I have to put `(el-get)' after `(req-package-finish)', because I install
  ;; some packages by `el-get-bundle', if I run `(el-get)' first, `(el-get)'
  ;; would raise an error as `el-get' doesn't know the local recipes at that
  ;; time.
  ;;
  ;; https://github.com/dimitri/el-get/issues/2232
  ;; https://github.com/dimitri/el-get/issues/2532
  (dashboard-setup-startup-hook)
  ;; Load dashboard in `after-init-hook'.
  ;;
  ;; `dashboard-setup-startup-hook' needs to configure `after-init-hook' and
  ;; `emacs-startup-hook'.
  )


;; `$after-init-hook' should be added at the end because it should be
;; run after `color-theme-backup-original-values'.
(add-hook 'after-init-hook '$after-init-hook t)

(load "~/.emacs_local.el" t)
