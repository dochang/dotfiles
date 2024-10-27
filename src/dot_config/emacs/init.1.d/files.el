(setup files

  ;; Avoid killing emacs by mistake
  (setq confirm-kill-emacs 'yes-or-no-p)

  ;; Don't add the final newline globally.
  ;;
  ;; Use editorconfig to force Emacs to add the final newline in certain files.
  ;;
  ;; [[info:emacs#Customize%20Save]]
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil)

  ;; Disable backup when saving.
  ;; [[info:emacs#Backup]]
  (setq make-backup-files nil)
  (setq version-control nil)

  ;; Customizing `safe-local-variable-values`.
  (setf (alist-get 'safe-local-variable-values **defaults**)
        (copy-alist safe-local-variable-values))
  (setq safe-local-variable-values
        (cons (cons 'buffer-auto-save-file-name nil)
              (copy-alist safe-local-variable-values)))

  )
