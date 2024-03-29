(req-package exec-path-from-shell
  :unless (memq system-type '(windows-nt ms-dos))
  ;; Do not load this package on Windows & MSDOS.
  :hook (emacs-startup . exec-path-from-shell-initialize)
  :init
  ;; Sometimes we have to set environment variables in rc files.  Disable the
  ;; warning.
  (setq exec-path-from-shell-check-startup-files nil)
  ;; Do not set `MANPATH` here.  Set man path in `~/.manpath`.
  (setq exec-path-from-shell-variables '("PATH")))
