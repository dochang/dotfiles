;; Configure guix-build-log only if emacs-guix is installed, otherwise the
;; hooks will be broken.
(when (locate-library "guix-build-log")
  (req-package guix-build-log
    :ensure (guix :pin :external)
    :hook ((shell-mode . guix-build-log-minor-mode))
    ))
