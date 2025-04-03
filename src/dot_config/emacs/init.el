;;; -*- lexical-binding: t; -*-


(let ((user-site-lisp-dir (locate-user-emacs-file "site-lisp")))
  (when (file-directory-p user-site-lisp-dir)
    (let ((default-directory user-site-lisp-dir))
      (normal-top-level-add-subdirs-to-load-path))))


(load (locate-user-emacs-file "bootstrap"))


;;; Specify custom file, but not load it.
;;; [[info:emacs#Saving%20Customizations]]
(with-eval-after-load 'cus-edit
  (setopt custom-file (locate-user-emacs-file ".emacs-custom.el")))
;; As init file may use write customization to `custom-file', we have to set
;; this variable at the beginning.


;;; System Environment Variables
;; Put any `setenv' here.


(defvar package-alist)
(declare-function package-delete "ext:package")
(defun $clean-up-user-packages ()
  (interactive)
  (mapc (lambda (pkg)
          (ignore-errors (package-delete pkg)))
        (apply 'append (mapcar 'cddr package-alist))))


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
  (:when-loaded
    (setopt load-dir-recursive t)
    (setopt load-dirs (locate-user-emacs-file "init.d"))))

(defun $load-local-config (name)
  (let ((local-path (expand-file-name name "~")))
    (when (file-exists-p local-path)
      (load-file local-path))))

;; Load all ".emacs_local_*.el", which are ordered by hook depth.
(mapc
 (lambda (local-file)
   (add-hook 'emacs-startup-hook
             `(lambda ()
                ($load-local-config ,(car local-file)))
             (cdr local-file)))
 (let ((match "\\`\\.emacs_local_\\(.*\\)\\.el\\'"))
   (seq-reduce
    (lambda (local-files local-name)
      (let ((matchp (string-match match local-name)))
        (if matchp
            (cons (cons local-name
                        (string-to-number (match-string 1 local-name)))
                  local-files)
          local-files)))
    (directory-files (expand-file-name "~") nil match)
    '())))

;; Load ".emacs_local.el" as default hook depth
(add-hook 'emacs-startup-hook
          '(lambda ()
             ($load-local-config ".emacs_local.el")))
