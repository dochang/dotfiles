(req-package exec-path-from-shell
  :init
  ;; Sometimes we have to set environment variables in rc files.  Disable the
  ;; warning.
  (setq exec-path-from-shell-check-startup-files nil)
  :if (memq window-system '(ns x))
  :config
  (exec-path-from-shell-initialize))
