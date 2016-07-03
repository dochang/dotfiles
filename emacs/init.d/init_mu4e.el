(req-package mu4e
  :loader :path
  :init
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads/")))
