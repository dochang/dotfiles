(req-package guix-prettify
  :ensure (guix :pin :external)
  :hook ((emacs-startup . global-guix-prettify-mode)
         (shell-mode . guix-prettify-mode)
         (dired-mode . guix-prettify-mode)))
