(req-package focus
  :hook (((prog-mode text-mode) . focus-mode)
         ((org-mode sh-mode yaml-mode ruby-mode) . (lambda () (focus-mode -1))))
  )
