;; Only available when Emacs 26+.
(unless (version< emacs-version "26")
  (req-package display-line-numbers
    :ensure (display-line-numbers :pin :built-in)
    :hook ((prog-mode adoc-mode conf-mode org-mode) . display-line-numbers-mode)
    :custom
    (display-line-numbers-type t)
    (display-line-numbers-grow-only nil)
    (display-line-numbers-current-absolute nil)
    (display-line-numbers-widen nil)
    ))
