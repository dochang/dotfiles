;; Configure guix-prettify only if emacs-guix is installed, otherwise the
;; hooks will be broken.
(when (locate-library "guix-prettify")
  (req-package guix-prettify
    :ensure (guix :pin :external)
    :hook ((emacs-startup . global-guix-prettify-mode)
           (shell-mode . guix-prettify-mode)
           (dired-mode . guix-prettify-mode))))
