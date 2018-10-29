(req-package newcomment
  :ensure (newcomment :pin :built-in)
  :custom
  ;; Use `fill-column' for `comment-indent'.
  (comment-fill-column nil)
  )
