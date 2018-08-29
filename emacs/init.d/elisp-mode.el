;;; Emacs Lisp Mode

(defun $init-file-p (file-name)
  (setq file-name (expand-file-name ($buffer-file-name file-name)))
  (cl-some (lambda (pattern) ($file-name-match pattern file-name))
           (list "[]>:/\\]\\..*\\(emacs\\|gnus\\|viper\\)\\(\\.el\\)?\\'"
                 "\\`\\..*emacs\\(\\.el\\)?\\'"
                 "[:/]_emacs\\(\\.el\\)?\\'"
                 "[]>:/\\]\\..*\\(emacs_local\\)\\(\\.el\\)?\\'"
                 (rx buffer-start
                     (eval (file-name-as-directory
                            (expand-file-name user-emacs-directory)))))))

(defun $emacs-lisp-mode-hook ()
  ($lisp-mode-common-hook)
  ;; Flycheck is too strict for my `.emacs' now.  Use flymake instead.
  (when (or (null buffer-file-name)
            ($init-file-p buffer-file-name)
            (not ($file-name-match "\\.el\\'" buffer-file-name)))
    (flycheck-mode -1)
    (flymake-mode 1)))

(req-package elisp-mode
  :ensure nil

  :mode ("Cask\\'" . emacs-lisp-mode)

  :hook (emacs-lisp-mode . $emacs-lisp-mode-hook)

  :init
  ;; Paredit always inserts a space when I insert "(" after ",@".  Change the
  ;; syntax descriptor of "@" from "_" to "'" will solve this problem.
  ;;
  ;; References:
  ;;
  ;;   - `paredit-space-for-delimiter-p'
  ;;   - `emacs-lisp-mode-syntax-table'
  (modify-syntax-entry ?@ "'   " emacs-lisp-mode-syntax-table)

  ;; Do not limit the output when evaluating.
  (setq eval-expression-print-length nil)
  (setq eval-expression-print-level nil))