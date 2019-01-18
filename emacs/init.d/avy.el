(req-package avy

  :hook (emacs-startup . avy-setup-default)

  :init

  (defhydra $hydra-avy (:color teal)
    "avy"
    (":" avy-goto-char)
    ("'" avy-goto-char-2)
    ("f" avy-goto-line)
    ("w" avy-goto-word-1)
    ("e" avy-goto-word-0)
    ("h" avy-org-goto-heading-timer)
    ("r" avy-org-refile-as-child)
    ("C-j" avy-resume)))
