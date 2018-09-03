(req-package menu-bar
  :require dired
  :ensure nil
  :init
  ;; use "K" to kill dired buffer.
  (unless (lookup-key dired-mode-map "K")
    (define-key dired-mode-map "K" 'kill-this-buffer)))
