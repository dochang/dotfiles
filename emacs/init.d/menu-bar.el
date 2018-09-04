(req-package menu-bar
  :after dired
  :ensure nil
  :commands (kill-this-buffer)
  :init
  ;; use "K" to kill dired buffer.
  (unless (lookup-key dired-mode-map "K")
    (define-key dired-mode-map "K" #'kill-this-buffer)))
