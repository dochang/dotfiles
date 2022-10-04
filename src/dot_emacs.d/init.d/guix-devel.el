;; Configure guix-devel only if emacs-guix is installed, otherwise the hooks
;; will be broken.
(when (locate-library "guix-devel")
  (req-package guix-devel
    :ensure (guix :pin :external)

    :hook ((scheme-mode . guix-devel-mode))

    :config

    (with-eval-after-load 'ffap
      (add-to-list 'ffap-alist '("\\.patch" . guix-devel-ffap-patch)))

    ))
