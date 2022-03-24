(req-package hydra
  :bind (("C-c x" . $hydra-extended/body))
  :config
  (defhydra $hydra-extended (:color teal)
    "extended map"
    ("M-o" ace-window "ace-window")
    ("a" $hydra-avy/body "avy")
    ("c" mc/edit-lines "edit multiple lines")
    ("l" multi-line "multi-line")
    ("p" poporg-dwim "poporg")
    ("e" emms-play-dired "play in dired")
    ("r" dired-rsync "rsync in dired")
    ("o" clm/toggle-command-log-buffer "Toggle the command log window")
    ("=" er/expand-region "expand region")
    ("s" $hydra-symbol-overlay/body "symbol overlay")
    ("t" ascii-table "ascii table")
    ("q" jq-interactively "Runs jq interactively")))
