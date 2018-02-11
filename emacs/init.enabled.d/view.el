;;; View Mode

(req-package view
  ;; Bind "less"-like key bindings
  :bind (:map view-mode-map
         ("N" . View-search-last-regexp-backward)
         ("?" . View-search-regexp-backward)
         ("G" . View-goto-line-last)
         ("k" . View-scroll-line-backward)
         ("j" . View-scroll-line-forward)
         ("b" . View-scroll-page-backward)
         ("f" . View-scroll-page-forward))
  :init
  ;; Enter View mode when a buffer become read-only.
  (setq view-read-only t))
