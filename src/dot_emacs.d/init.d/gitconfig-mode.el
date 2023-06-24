;; Also edit config file of bare repo by `gitconfig-mode'.

(req-package gitconfig-mode
  :ensure git-modes
  :mode ("\\.git/config\\'" . gitconfig-mode))
