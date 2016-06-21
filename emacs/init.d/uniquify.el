;;; Make Buffer Names Unique
;;; [[info:emacs#Uniquify]]

(req-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward))
