(req-package dumb-jump
  :bind (("C-c j" . $hydra-dumb-jump/body))

  :init

  (setq dumb-jump-default-project "~/src")

  (defhydra $hydra-dumb-jump (:color teal)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))
