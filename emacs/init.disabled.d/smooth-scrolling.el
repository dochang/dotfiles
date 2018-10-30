;; Do not use `smooth-scrolling'.  It doesn't work well with `beginend' in
;; `dired' mode.

(req-package smooth-scrolling
  :hook (emacs-startup . smooth-scrolling-mode)
  :custom
  (smooth-scroll-margin 4))
