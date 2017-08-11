(req-package symbol-overlay
  :bind (:map symbol-overlay-map
         ("M-n" . symbol-overlay-switch-forward)
         ("M-p" . symbol-overlay-switch-backward)
         ("m" . symbol-overlay-mode)
         ("DEL" . symbol-overlay-remove-all))
  :init
  (defhydra $hydra-symbol-overlay (:color teal)
    "symbol overlay"
    ("i" symbol-overlay-put "put")
    ("n" symbol-overlay-switch-forward "switch forward")
    ("p" symbol-overlay-switch-backward "switch backward")
    ("m" symbol-overlay-mode "symbol overlay mode")
    ("DEL" symbol-overlay-remove-all "remove all")
    ("q" nil "quit")))
