(req-package macrostep
  :after elisp-mode

  :init

  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)
  (define-key lisp-interaction-mode-map (kbd "C-c e") 'macrostep-expand)

  )
