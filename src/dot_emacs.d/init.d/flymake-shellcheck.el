(when (not (version< emacs-version "26"))
  (req-package flymake-shellcheck
    :hook (sh-mode . flymake-shellcheck-load)))
