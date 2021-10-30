(req-package visual-fill-column
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))
