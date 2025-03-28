(setup files

  ;; Avoid killing emacs by mistake
  (setopt confirm-kill-emacs 'yes-or-no-p)

  ;; Don't add the final newline globally.
  ;;
  ;; Use editorconfig to force Emacs to add the final newline in certain files.
  ;;
  ;; [[info:emacs#Customize%20Save]]
  (setopt require-final-newline nil)
  (setopt mode-require-final-newline nil)

  ;; Disable backup when saving.
  ;; [[info:emacs#Backup]]
  (setopt make-backup-files nil)
  (setopt version-control nil)

  ;; Customizing `safe-local-variable-values`.
  (unless (alist-get 'safe-local-variable-values **defaults**)
    (setf (alist-get 'safe-local-variable-values **defaults**)
          (copy-alist safe-local-variable-values)))
  (setopt safe-local-variable-values
          (seq-reduce
           (lambda (alist elem)
             (if (seq-contains-p alist elem)
                 alist
               (cons elem alist)))
           '((buffer-auto-save-file-name))
           safe-local-variable-values))

  )
