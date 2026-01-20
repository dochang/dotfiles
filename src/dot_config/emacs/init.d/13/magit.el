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

  (with-eval-after-load 'git-commit
    ;; `git-commit' has been merged into `magit'.  It must not be installed, in
    ;; order to load the proper `git-commit' library from `magit'.
    ;;
    ;; https://github.com/magit/magit/commit/9be8a4ab7a7dc6f34615f1011e8da263651c8f87

    (keymap-set git-commit-redundant-bindings "C-M-i" #'completion-at-point)
    ;; For conventional-commit

    )

  (with-eval-after-load 'magit-mode

    ;; Don't pop a new window.
    (setopt magit-display-buffer-function
            #'magit-display-buffer-same-window-except-diff-v1)

    )

  (with-eval-after-load 'magit-status

    (setopt magit-status-show-hashes-in-headers t)

    )

  )
