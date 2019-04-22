(req-package scroll-bar
  :ensure (scroll-bar :pin :built-in)
  :hook (emacs-startup . (lambda ()
                           (when (fboundp 'set-scroll-bar-mode)
                             ;; If Emacs is built with `--without-x',
                             ;; `set-scroll-bar-mode' will be undefined.
                             (set-scroll-bar-mode 'left)))))
