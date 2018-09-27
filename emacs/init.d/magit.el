;;; Magit
;; [[http://magit.github.com/magit/]]
;; [[http://github.com/magit/magit]]
;; [[http://daemianmack.com/magit-cheatsheet.html]]

(req-package magit
  :bind (("C-c g" . magit-status))
  :custom
  ;; Don't pop a new window.
  (magit-status-buffer-switch-function 'switch-to-buffer))
