(req-package bln-mode
  :bind (("C-c b" . $hydra-bln/body))

  :init

  (defhydra $hydra-bln (:color amaranth)
    "Binary line navigation mode"
    ("h" bln-backward-half "Backward in line")
    ("l" bln-forward-half "Forward in line")
    ("k" bln-backward-half-v "Backward in window")
    ("j" bln-forward-half-v "Forward in window")
    ("b" bln-backward-half-b "Backward in buffer")
    ("f" bln-forward-half-b "Forward in buffer")
    ("q" nil "Quit")))
