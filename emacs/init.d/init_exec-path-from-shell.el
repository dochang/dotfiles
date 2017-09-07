(req-package exec-path-from-shell
  :init
  ;; Sometimes we have to set environment variables in rc files.  Disable the
  ;; warning.
  (setq exec-path-from-shell-check-startup-files nil)
  ;; Do not set `MANPATH` here.  Set man path in `~/.manpath`.
  (setq exec-path-from-shell-variables '("PATH"))
  :if (memq window-system '(ns x))
  :config
  (exec-path-from-shell-initialize))
