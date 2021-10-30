(req-package locate

  :after dired
  ;; `:after' is required because `locate-ls-subdir-switches' depends on
  ;; `dired-listing-switches', which must be set before setting
  ;; `locate-ls-subdir-switches'.

  :config
  (setq locate-ls-subdir-switches dired-listing-switches)
  ;; `locate-ls-subdir-switches` should default to the value of
  ;; `dired-listing-switches'.
  ;;
  ;; This variable has to be set in `:config' section since it depends on
  ;; `dired-listing-switches', which may be set after `dired' loaded, because
  ;; `dired' is an essential package which may be loaded during the startup.
  ;; So we have to set this variable after `locate' loaded.

  )
