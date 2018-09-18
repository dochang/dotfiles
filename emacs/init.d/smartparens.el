(req-package smartparens

  :hook ((emacs-startup . smartparens-global-strict-mode)
         (emacs-startup . show-smartparens-global-mode))

  :custom

  (sp-base-key-bindings nil)
  ;; Do not use key bindings of neither paredit style nor smartparens style.
  ;; They both bind too many keys.

  (sp-override-key-bindings
   '(

     ("C-M-f" . sp-forward-sexp)
     ("C-M-b" . sp-backward-sexp)
     ("C-M-u" . sp-backward-up-sexp)
     ("C-M-d" . sp-down-sexp)

     ("C-M-k" . sp-kill-sexp)
     ("C-M-SPC" . sp-mark-sexp)

     ;; Do not bind these keys.  They have been bound to other commands.
     ;;
     ;; ("C-<right>" . sp-forward-slurp-sexp)
     ;; ("C-<left>" . sp-forward-barf-sexp)
     ;; ("C-M-<left>" . sp-backward-slurp-sexp)
     ;; ("C-M-<right>" . sp-backward-barf-sexp)

     ("C-)" . sp-forward-slurp-sexp)
     ("C-}" . sp-forward-barf-sexp)
     ("C-(" . sp-backward-slurp-sexp)
     ("C-{" . sp-backward-barf-sexp)

     ))

  :config

  (require 'smartparens-config))
