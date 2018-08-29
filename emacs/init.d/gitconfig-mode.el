;; Also edit config file of bare repo by `gitconfig-mode'.

(req-package gitconfig-mode
  :mode ("\\.git/config\\'" . gitconfig-mode))
