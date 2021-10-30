(req-package macrostep-geiser
  :init

  (eval-after-load 'geiser-mode
    '(add-hook 'geiser-mode-hook 'macrostep-geiser-setup))

  (eval-after-load 'geiser-repl
    '(add-hook 'geiser-repl-mode-hook 'macrostep-geiser-setup))

  (eval-after-load 'cider-mode
    '(add-hook 'cider-mode-hook 'macrostep-geiser-setup))

  )

;; (req-package macrostep-geiser
;;   :after geiser-mode
;;   :init
;;   (add-hook 'geiser-mode-hook 'macrostep-geiser-setup))

;; (req-package macrostep-geiser
;;   :after geiser-repl
;;   :init
;;   )

;; (req-package macrostep-geiser
;;   :after cider-mode
;;   :init
;;   )
