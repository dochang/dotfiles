(setup (:package beacon)

  (add-hook 'emacs-startup-hook 'beacon-mode)

  (define-advice keyboard-quit (:before () blink-cursor)
    (beacon-blink))

  (:when-loaded

    (:option beacon-blink-when-point-moves-vertically nil)
    (:option beacon-blink-when-point-moves-horizontally nil)
    (:option beacon-blink-when-buffer-changes t)
    (:option beacon-blink-when-window-scrolls t)
    (:option beacon-blink-when-window-changes t)
    (:option beacon-blink-when-focused t)

    )

  )
