;;; Man

(req-package man
  :init
  ;; Make the manpage the current buffer in the current window
  (setq Man-notify-method 'pushy))
