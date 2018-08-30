(req-package newcomment
  :ensure nil
  :init
  ;; Use `fill-column' for `comment-indent'.
  (setq comment-fill-column nil)
  )
