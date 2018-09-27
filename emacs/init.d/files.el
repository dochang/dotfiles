(req-package files

  :ensure nil

  :custom

  ;; Avoid killing emacs by mistake
  (confirm-kill-emacs 'yes-or-no-p)

  ;; Don't add the final newline globally.
  ;;
  ;; Use editorconfig to force Emacs to add the final newline in certain files.
  ;;
  ;; [[info:emacs#Customize%20Save]]
  (require-final-newline nil)
  (mode-require-final-newline nil)

  ;; Disable backup when saving.
  ;; [[info:emacs#Backup]]
  (make-backup-files nil)
  (version-control nil)

  :init

  ;; Customizing `safe-local-variable-values`.
  (defvar **default-safe-local-variable-values**
    (copy-alist safe-local-variable-values))
  (setq safe-local-variable-values
        (cons (cons 'buffer-auto-save-file-name nil)
              **default-safe-local-variable-values**))

  )
