;;; Magit
;; [[http://magit.github.com/magit/]]
;; [[http://github.com/magit/magit]]
;; [[http://daemianmack.com/magit-cheatsheet.html]]

(req-package magit
  :bind (("C-c g" . magit-status))
  :init
  (setq magit-status-show-hashes-in-headers t)
  ;; Don't pop a new window.
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  ;; Enable global key bindings
  (setq magit-define-global-key-bindings t)
  )
