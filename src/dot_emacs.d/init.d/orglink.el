(req-package orglink
  :disabled
  ;; Disabled.  Use org-link-minor-mode instead.  It uses `org-highlight-links'
  ;; and supports more link types.
  :hook (emacs-startup . global-orglink-mode)
  :init
  (setq orglink-activate-links '(bracket angle plain))
  (setq orglink-activate-in-modes '(emacs-lisp-mode)))
