(req-package find-dired
  :require dired
  ;; `:require' is required because `find-ls-subdir-switches' depends on
  ;; `dired-listing-switches', which must be set before setting
  ;; `find-ls-subdir-switches'.
  :init
  (setq find-ls-option '("-exec ls -lhAd {} +" . "-lhAd"))
  ;; `find-ls-option' defaults to '("-ls" . "-dilsb").  Because
  ;; `-ls' quotes non-printable characters in file names using
  ;; C-like backslash escapes, We have to pass custom parameters
  ;; to `ls' here.
  (setq find-ls-subdir-switches dired-listing-switches)
  ;; `find-ls-subdir-switches' should default to the value of
  ;; `dired-listing-switches'.  DO NOT set it to `nil',
  ;; otherwise `dired-subdir-switches' in `*Find*' buffer is
  ;; `nil', then `(cdr find-ls-option)' will be used.
  )
