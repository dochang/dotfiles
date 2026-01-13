(setup (:package envrc)

  (add-hook 'emacs-startup-hook 'envrc-global-mode 99)
  ;; Put `envrc-global-mode' into the end of the hook in order to ensure
  ;; `envrc-mode' to be initialized before other buffer minor modes.

  )
