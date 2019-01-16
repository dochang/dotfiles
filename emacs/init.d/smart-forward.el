(req-package smart-forward

  :bind (("C-c f" . $hydra-smart-forward/body))

  :commands (smart-forward
             smart-backward
             smart-up
             smart-down)

  :init

  (defhydra $hydra-smart-forward (:color amaranth)
    "smart-forward"
    ("f" smart-forward "forward")
    ("b" smart-backward "backward")
    ("u" smart-up "up")
    ("d" smart-down "down")
    ("q" nil "quit"))

  )
