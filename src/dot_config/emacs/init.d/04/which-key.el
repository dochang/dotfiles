(setup (:package which-key)

  (add-hook 'emacs-startup-hook #'which-key-mode)

  (:when-loaded

    (which-key-setup-minibuffer)
    ;; Display which-key in `side-window' does not work well with
    ;; `repeat-help-mode'.  Display which-key in minibuffer instead.

    (setopt which-key-use-C-h-commands nil)

    (setopt which-key-side-window-max-width 0.333)

    (setopt which-key-sort-order 'which-key-key-order-alpha)

    )

  )
