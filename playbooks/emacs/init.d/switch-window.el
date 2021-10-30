(req-package switch-window

  :require hydra

  :bind (("C-c w" . $hydra-switch-window/body))

  :init

  (setq switch-window-auto-resize-window t)

  (setq switch-window-input-style 'minibuffer)

  (setq switch-window-shortcut-appearance 'asciiart)

  (defhydra $hydra-switch-window (:color teal)
    "switch window"
    ("o" switch-window "switch")
    ("1" switch-window-then-maximize "switch then maximize")
    ("2" switch-window-then-split-below "switch then split below")
    ("3" switch-window-then-split-right "switch then split right")
    ("0" switch-window-then-delete "switch then delete")

    ("4" $hydra-switch-window-then/body "switch then do sth"))

  (defhydra $hydra-switch-window-then (:color teal)
    "switch window then do sth"
    ("d" switch-window-then-dired "switch then dired")
    ("f" switch-window-then-find-file "switch then find file")
    ("m" switch-window-then-compose-mail "switch then compose mail")
    ("r" switch-window-then-find-file-read-only
     "switch then find file read only")

    ("C-f" switch-window-then-find-file "switch then find file")
    ("C-o" switch-window-then-display-buffer "switch then display buffer")

    ("0" switch-window-then-kill-buffer "switch then kill buffer"))

  )
