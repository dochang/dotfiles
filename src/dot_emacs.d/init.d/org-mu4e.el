(req-package org-mu4e
  :ensure (mu4e :pin :external)

  :bind (:map mu4e-headers-mode-map
         ("C-c c" . org-mu4e-store-and-capture)
         :map mu4e-view-mode-map
         ("C-c c" . org-mu4e-store-and-capture)))
