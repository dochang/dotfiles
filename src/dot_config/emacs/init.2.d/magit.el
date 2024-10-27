;;; Magit
;; [[http://magit.github.com/magit/]]
;; [[http://github.com/magit/magit]]
;; [[http://daemianmack.com/magit-cheatsheet.html]]

(setup (:package magit)

  ;; Enable global key bindings
  (setq magit-define-global-key-bindings 'recommended)

  (:when-loaded

    (:option magit-status-show-hashes-in-headers t)

    ;; Don't pop a new window.
    (:option magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

    )

  )
