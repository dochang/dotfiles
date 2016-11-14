(req-package web-mode-edit-element
  :require web-mode
  :init
  (add-hook 'web-mode-hook 'web-mode-edit-element-minor-mode))
