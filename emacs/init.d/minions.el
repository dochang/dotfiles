(when (not (version< emacs-version "25.2"))
  (req-package minions
    :hook (emacs-startup . minions-mode)))
