;;; Make Buffer Names Unique
;;; [[info:emacs#Uniquify]]

(req-package uniquify
  :ensure nil
  :hook (emacs-startup . (lambda () (require 'uniquify)))
  :init
  (setq uniquify-buffer-name-style 'post-forward))
