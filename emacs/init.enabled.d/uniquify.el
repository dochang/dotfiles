;;; Make Buffer Names Unique
;;; [[info:emacs#Uniquify]]

(req-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'post-forward))
