(req-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
         ("i" . dired-subtree-insert)
         (";" . dired-subtree-remove)))
