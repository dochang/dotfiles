(req-package guix-devel
  :ensure (guix :pin :external)

  :hook ((scheme-mode . guix-devel-mode))

  :config

  (with-eval-after-load 'ffap
    (add-to-list 'ffap-alist '("\\.patch" . guix-devel-ffap-patch)))

  )
