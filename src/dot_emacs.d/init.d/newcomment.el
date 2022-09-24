(req-package newcomment
  :ensure (newcomment :pin :built-in)
  :init
  ;; Use `fill-column' for `comment-indent'.
  (setq comment-fill-column nil)
  )
