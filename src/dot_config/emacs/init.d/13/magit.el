;;; Magit
;; [[http://magit.github.com/magit/]]
;; [[http://github.com/magit/magit]]
;; [[http://daemianmack.com/magit-cheatsheet.html]]

(setup (:package magit)

  (setq magit-define-global-key-bindings nil)
  ;; Do not enable key bindings automatically.

  (add-hook 'emacs-startup-hook
            (lambda ()
              (mapc (lambda (set)
                      (let ((magit-define-global-key-bindings set))
                        (magit-maybe-define-global-key-bindings 'force)))
                    '(recommended default))))
  ;; Enable all key binding sets manually.
  ;;
  ;; `magit-maybe-define-global-key-bindings' may be run in `after-init-hook'
  ;; and before this config.  `magit-define-global-key-bindings' can not be
  ;; set in `eval-after-load'.

  )
