(setup (:package super-save)

  (add-hook 'emacs-startup-hook #'super-save-mode)

  (:when-loaded

    (setopt super-save-auto-save-when-idle t)

    (setopt auto-save-default nil)

    (setopt super-save-triggers
            (seq-reduce
             (lambda (triggers trigger)
               (if (seq-contains-p triggers trigger)
                   triggers
                 (cons trigger triggers)))
             '(switch-to-buffer-other-window
               switch-to-buffer-other-frame
               switch-to-buffer-other-tab
               tab-next
               tab-previous
               magit-status)
             super-save-triggers))

    )

  )
