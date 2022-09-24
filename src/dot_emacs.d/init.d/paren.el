(eval-and-compile
  (require 'req-package))

(req-package paren
  :hook (emacs-startup . show-paren-mode))
