;;; -*- lexical-binding: t; -*-


(let ((user-site-lisp-dir (locate-user-emacs-file "site-lisp")))
  (when (file-directory-p user-site-lisp-dir)
    (let ((default-directory user-site-lisp-dir))
      (normal-top-level-add-subdirs-to-load-path))))


(load (locate-user-emacs-file "bootstrap"))


;;; Specify custom file, but not load it.
;;; [[info:emacs#Saving%20Customizations]]
(setq custom-file (locate-user-emacs-file ".emacs-custom.el"))
;; As init file may use write customization to `custom-file', we have to set
;; this variable at the beginning.


;;; System Environment
;; Git cannot detect if it's run in Emacs.
(setenv "GIT_PAGER" "")


(defvar package-alist)
(declare-function package-delete "ext:package")
(defun $clean-up-user-packages ()
  (interactive)
  (mapc (lambda (pkg)
          (ignore-errors (package-delete pkg)))
        (apply 'append (mapcar 'cddr package-alist))))

(defun $package-user-installed-p (pkg)
  "Return non-nil if PKG is in ELPA directory.

If a package is installed as external or built-in, this function
returns nil.

If `package-archives' is modified, PKG may considered as
obsolete, this function returns non-nil."
  (and (package-installed-p pkg)
       (package--user-installed-p pkg)))

(defun $package-install-from-elpa (pkg)
  "Install PKG if it's not installed from any ELPA source.

It's impossible to `package-install' an external package from an
ELPA source even if `package-install-upgrade-built-in' is true,
because Emacs considers external packages are installed but not
built-in."
  (package--archives-initialize)
  (let ((desc (cadr (assq pkg package-archive-contents))))
    (when desc
      (package-install desc))))


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


(setup grand-cross

  (:package vline)

  (define-minor-mode $grand-cross-mode
    "A minor mode to highlight current line and column."
    :init-value nil
    (let ((arg (if $grand-cross-mode 1 -1)))
      (hl-line-mode arg)
      (vline-mode arg)))

  ;; (:with-mode (prog-mode conf-mode)
  ;;   (:hook '$grand-cross-mode))
  ;; Do not enable it by default.

  )


(setup (:package load-dir)

  (setq load-dirs
        (directory-files user-emacs-directory 'full "init\\..*\\.d"))

  )

(defun $load-local-config (name)
  (let ((local-path (expand-file-name name "~")))
    (when (file-exists-p local-path)
      (load-file local-path))))

(mapc
 (lambda (local-file)
   (add-hook 'emacs-startup-hook
             `(lambda ()
                ($load-local-config ,(car local-file)))
             (cdr local-file)))
 (seq-reduce
  (lambda (local-files local-name)
    (let ((matchp (string-match "\\`\\.emacs_local_\\(.*\\)\\.el\\'" local-name)))
      (if matchp
          (cons (cons local-name
                      (string-to-number (match-string 1 local-name)))
                local-files)
        local-files)))
  (directory-files (expand-file-name "~"))
  '()))

(add-hook 'emacs-startup-hook
          '(lambda ()
             ($load-local-config ".emacs_local.el")))
