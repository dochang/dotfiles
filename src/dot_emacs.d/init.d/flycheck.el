(req-package flycheck

  :hook (prog-mode . flycheck-mode)

  :init

  (setq flycheck-disabled-checkers '())

  (setq flycheck-emacs-lisp-load-path 'inherit)
  ;; Make sure flycheck can load compile time macros.
  ;;
  ;; https://emacs.stackexchange.com/a/18095

  :config

  (flycheck-yamllint-setup)

  (flycheck-package-setup)

  )
