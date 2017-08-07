(req-package expand-region
  :bind (("C-=" . er/expand-region)
         :map $extended-map
         ("=" . er/expand-region)))
