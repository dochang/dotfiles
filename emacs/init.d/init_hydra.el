(req-package hydra
  :bind (("C-c x" . $hydra-extended/body))
  :config
  (defhydra $hydra-extended (:color teal)
    "extended map"
    ("c" mc/edit-lines "edit multiple lines")
    ("l" multi-line "multi-line")
    ("e" emms-play-dired "play in dired")
    ("=" er/expand-region "expand region")
    ("s" $hydra-symbol-overlay/body "symbol overlay")))
