;; Eldoc Mode
;; [[info:emacs#Lisp%20Doc]]

(req-package eldoc
  :hook ((emacs-lisp-mode) . eldoc-mode))
