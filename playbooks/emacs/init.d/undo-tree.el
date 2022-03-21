(req-package undo-tree
  :hook (emacs-startup . global-undo-tree-mode)
  :init
  (setq undo-tree-auto-save-history nil)
  ;; DO NOT persist undo history.  I don't need it.
  )
