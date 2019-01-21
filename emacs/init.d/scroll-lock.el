(req-package scroll-lock
  :bind ([Scroll_Lock] . scroll-lock-mode)
  :hook ((help-mode Info-mode Man-mode helpful-mode) . scroll-lock-mode))
