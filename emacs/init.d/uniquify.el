;;; Make Buffer Names Unique
;;; [[info:emacs#Uniquify]]

(req-package uniquify
  :ensure (uniquify :pin :built-in)
  :hook (emacs-startup . (lambda () (require 'uniquify)))
  :custom
  (uniquify-buffer-name-style 'post-forward))
