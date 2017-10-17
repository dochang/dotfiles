(req-package dired-subtree
  :require dired
  :bind (:map dired-mode-map
         ("i" . dired-subtree-insert)
         (";" . dired-subtree-remove)))
