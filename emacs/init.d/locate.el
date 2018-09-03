(req-package locate
  :require dired
  ;; `:require' is required because `locate-ls-subdir-switches' depends on
  ;; `dired-listing-switches', which must be set before setting
  ;; `locate-ls-subdir-switches'.
  :init
  (setq locate-ls-subdir-switches dired-listing-switches)
  ;; `locate-ls-subdir-switches` should default to the value of
  ;; `dired-listing-switches'.
  )
