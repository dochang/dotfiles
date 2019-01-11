(req-package orglink
  :disabled
  ;; Disabled.  Use org-link-minor-mode instead.  It uses `org-highlight-links'
  ;; and supports more link types.
  :hook (emacs-startup . global-orglink-mode)
  :custom
  (orglink-activate-links '(bracket angle plain))
  (orglink-activate-in-modes '(emacs-lisp-mode)))
