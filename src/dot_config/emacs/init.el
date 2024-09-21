;;; -*- lexical-binding: t; -*-


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
        (mapcar
         (lambda (d) (expand-file-name (locate-user-emacs-file d)))
         '("init.0.d" "init.1.d")))

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
