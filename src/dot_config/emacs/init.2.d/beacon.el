(setup (:package beacon)

  (add-hook 'emacs-startup-hook 'beacon-mode)

  (with-eval-after-load 'simple

    (define-advice keyboard-quit (:before () blink-cursor)
      (beacon-blink))

    )

  (:when-loaded

    (setopt beacon-blink-when-point-moves-vertically nil)
    (setopt beacon-blink-when-point-moves-horizontally nil)
    (setopt beacon-blink-when-buffer-changes t)
    (setopt beacon-blink-when-window-scrolls t)
    (setopt beacon-blink-when-window-changes t)
    (setopt beacon-blink-when-focused t)

    )

  )
