(when (not (version< emacs-version "25.3"))
  (req-package beginend
    :hook (emacs-startup . beginend-global-mode)))
