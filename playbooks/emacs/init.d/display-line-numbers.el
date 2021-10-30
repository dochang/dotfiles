;; Only available when Emacs 26+.
(unless (version< emacs-version "26")
  (req-package display-line-numbers
    :ensure (display-line-numbers :pin :built-in)
    :hook ((prog-mode adoc-mode conf-mode) . display-line-numbers-mode)
    :init
    (setq display-line-numbers-type t)
    (setq display-line-numbers-grow-only nil)
    (setq display-line-numbers-current-absolute nil)
    (setq display-line-numbers-widen nil)
    ))
